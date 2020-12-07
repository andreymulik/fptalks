-- Persist DSL requirements
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving #-}

-- Type system features
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances, GADTs #-}

-- Advanced syntax extensions
{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

{- |
    Module      :  Foundation
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "Foundation" provides foundation type 'FPTalks' and handlers for all site
    pages.
-}
module Foundation
(
  -- * Website
  FPTalks (..), Handler, Widget,
  
  -- ** Request handling
  getMessengerR,
  
  -- ** DB definitions
  User, UserId, migrateAll, resourcesFPTalks
)
where

-- Web
import Yesod.Persist.Core
import Yesod.Auth.Email
import Yesod.Auth
import Yesod.Core
import Yesod.Form

import Decoration
import Sendmail

-- Database
import Database.Persist.Sqlite
import Database.Persist.TH

-- Basics
import Data.Typeable ( Typeable )
import Data.Maybe    (  isJust  )
import Data.Text     (   Text   )

import Control.Monad (   join   )

default ()

--------------------------------------------------------------------------------

{- Routing and request handling. -}

{-
  Template-based (DSL) route generator.
  
  Each line in this template is responsible for the path to the page(s) on the
  site. In this case, two options for processing requests are used:
  
  * @/@ only allows @GET@ requests that are processed by the 'getMessengerR'
  * @/auth/*@ is processed as a sub-site with own pages, requests and handlers,
  which are implemented by the @yesod-auth@ and 'Yesod.Auth.Email' plugin.
-}
mkYesod "FPTalks" [parseRoutes|
/     MessengerR GET
/auth AuthR      Auth getAuth
|]

-- | 'MessengerR' (<localhost:3000/>) @GET@ request handler.
getMessengerR :: Handler Html
getMessengerR =  defaultLayout $ do
  let title = "FPTalks messenger page"
  setTitle title
  [whamlet|
    <div#page-header>
      <h1>#{title}
      <p>Not implemented yet
    <div#page-content>
      <p.message-box>Content example
  |]

--------------------------------------------------------------------------------

{- Application representation and basic behavior. -}

{- |
  'FPTalks' is main site type, that represent whole web application. 'FPTalks'
  value contains @SQL@ connection representation.
-}
data FPTalks = FPTalks
  {
    -- | @persistent@ SQL backend.
    sqlBackend :: SqlBackend
  }

{- |
  Website instance for 'FPTalks' type, defines basic web application behavior.
  
  Type classes supports default definitions for most definitions, so I implement
  only the most necessary ones.
-}
instance Yesod FPTalks
  where
    -- | Page access control.
    isAuthorized (AuthR  _) _ = return Authorized
    isAuthorized MessengerR _ = isUser
    
    -- | The page to which the user will be redirected if he isn't logged in.
    authRoute _ = Just (AuthR LoginR)
    
    {- |
      Static site address, required for email registration (verification email
      must contain absolute path to server). Note that 'approot' defines the
      root for all URLs on the site, not the site address (this is the
      responsibility of the web server).
    -}
    approot = ApprootStatic "http://localhost:3000"
    
    -- | Prevent cross-site request forgery.
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
    
    {- |
      Converts a 'Widget' (related @HTML@, @CSS@ and @JS@ fragments) to a web
      page ('Handler'). Unlike the standard definition, it doesn't output
      session messages as page content.
    -}
    defaultLayout = customLayout
    
    -- | Converts an 'ErrorResponse' exception to an error page.
    errorHandler = customErrorW

-- | The page is available only if the user is logged in.
isUser :: Handler AuthResult
isUser =  AuthenticationRequired `maybe` const Authorized <$> maybeAuthId

--------------------------------------------------------------------------------

{- Database support. -}

{- |
  The glue between the database and the web application, lifts requests to the
  database associated with the application to the level of HTTP request
  processing.
-}
instance YesodPersist FPTalks
  where
    type YesodPersistBackend FPTalks = SqlBackend
    
    -- Run some action with 'FPTalks' @SQL@ backend
    runDB action = getYesod >>= runSqlConn action . sqlBackend

{- |
  The connecting link between the database and the authentication service. Adds
  one important utility function, extends 'YesodPersist'.
-}
instance YesodAuthPersist FPTalks

{-
  Composite @DSL@:
  
  * 'share' applies multiple @Template Haskell@ generators (first argument) to
  one list of entity definitions (second argument) and embeds the generated code
  into the module at compile time
  * 'persistLowerCase' converts @QuasiQuoter@ to entity definitions for
  @Template Haskell@ generators
  * 'mkPersist' is a code generator that defines types representing records in
  database tables
  * 'mkMigrate' creates a function that rebuilds databases when their
  corresponding definitions change.
-}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  email    Text
  password Text Maybe
  verkey   Text Maybe
  verified Bool
  UniqueUser email
  deriving Typeable
|]

--------------------------------------------------------------------------------

{- Auth support. -}

{- |
  Authentication service for 'Yesod', defines the behavior when login/logout of
  the account, the authentication process and plugins.
-}
instance YesodAuth FPTalks
  where
    -- | 'AuthId' for 'FPTalks' site is the same type as 'User' table key.
    type AuthId FPTalks = UserId
    
    loginHandler = customLoginW
    authPlugins  = const [authEmail]
    logoutDest   = const (AuthR LoginR)
    loginDest    = const MessengerR
    
    authenticate creds =
      let user = User (credsIdent creds) Nothing Nothing False
      in  liftHandler . runDB $ Authenticated . either entityKey id <$> insertBy user

{- |
  Authentication plugin for 'YesodAuth' service, defines some pages in
  @\/auth\/*@ subsite for @JSON@-based message exchange.
-}
instance YesodAuthEmail FPTalks
  where
    -- | 'AuthEmailId' for 'FPTalks' site is the same type as 'AuthId'.
    type AuthEmailId FPTalks = UserId
    
    registerHandler    = customRegisterW
    emailLoginHandler  = customEmailLoginW
    setPasswordHandler = customSetPasswordW
    
    afterPasswordRoute = logoutDest
    
    addUnverified email verkey = liftHandler . runDB . insert $
      User email Nothing (Just verkey) False
    
    getVerifyKey = liftHandler . runDB . (join . fmap userVerkey <$>) . get
    
    setVerifyKey uid key = liftHandler . runDB $
      update uid [UserVerkey =. Just key]
    
    getPassword = liftHandler . runDB . fmap (join . fmap userPassword) . get
    
    setPassword uid pass = liftHandler . runDB $
      update uid [UserPassword =. Just pass]
    
    verifyAccount u = liftHandler $ runDB $
      let verify = [UserVerified =. True, UserVerkey =. Nothing]
      in  return Nothing `maybe` const (Just u <$ update u verify) =<< get u
    
    sendVerifyEmail email _ verurl = sendEmail email verurl
    
    getEmailCreds email = liftHandler . runDB $ fmap emailCreds <$> getBy (UniqueUser email)
      where
        emailCreds (Entity uid u) = EmailCreds
          {
            emailCredsStatus = isJust (userPassword u),
            emailCredsVerkey = userVerkey u,
            emailCredsAuthId = Just uid,
            emailCredsEmail  = email,
            emailCredsId     = uid
          }
    
    getEmail = liftHandler . runDB . (fmap userEmail <$>) . get

--------------------------------------------------------------------------------

{- Internationalization stuff. -}

instance RenderMessage FPTalks FormMessage
  where
    renderMessage _ _ = defaultFormMessage



