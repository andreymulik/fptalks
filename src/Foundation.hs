-- Persist DSL requirements
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving #-}

-- Type system features
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances, GADTs #-}

-- Advanced syntax extensions
{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

{- |
    Module      :  Foundation
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "Foundation" provides foundation type 'FPTalks', SQL and Auth support,
    routes and handlers.
-}
module Foundation
(
  -- * Export
  module Yesod.Core,
  
  -- * Web
  FPTalks (..), Handler, Widget, resourcesFPTalks,
  
  -- * SQL
  User, UserId, MessageId, GroupsId, migrateAll
)
where

-- Web
import Yesod.Persist.Core
import Yesod.Auth.Email
import Yesod.Auth

import Yesod.Core hiding ( addMessage )
import Yesod.Form

import Decoration
import Sendmail

-- Database
import Database.Persist.Sqlite
import Database.Persist.TH

-- Basics
import Data.Typeable ( Typeable )
import Data.Coerce
import Data.Maybe
import Data.Text ( Text )
import Data.Int

import Text.Blaze ( ToMarkup (..) )

import Control.Monad.Trans.Reader
import Control.Monad ( void, join )

default ()

--------------------------------------------------------------------------------

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
  email      Text
  password   Text Maybe
  verkey     Text Maybe
  verified   Bool
  UniqueUser email
  deriving   Typeable

Group
  name  Text Maybe
  count Int
  deriving Typeable

Groups
  gid          GroupId
  member       UserId
  UniqueGroups gid member
  deriving     Typeable

Message
  text      Text
  sender    UserId
  receivers GroupId
  deriving  Typeable
|]

userField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m UserId
userField =  convertField toUserId fromUserId intField

fromUserId :: UserId -> Int64
fromUserId =  coerce

toUserId :: Int64 -> UserId
toUserId =  coerce

--------------------------------------------------------------------------------

instance ToMarkup UserId where toMarkup = toMarkup . fromUserId

--------------------------------------------------------------------------------

{- Application representation and basic behavior. -}

{- |
  'FPTalks' is main site type, that represent whole web application. 'FPTalks'
  value contains @SQL@ connection representation.
-}
data FPTalks = FPTalks {sqlBackend :: SqlBackend}

{-
  Template-based (DSL) route generator. Creates data type 'Route' 'FPTalks',
  which represents site routes.
-}
mkYesodData "FPTalks" $(parseRoutesFile "fptalks.routes")

--------------------------------------------------------------------------------

{- |
  Website instance for 'FPTalks' type, defines basic web application behavior.
  
  Type classes supports default definitions for most definitions, so I implement
  only the most necessary ones.
-}
instance Yesod FPTalks
  where
    -- | Page access control.
    isAuthorized (AuthR        _) _ = return Authorized
    isAuthorized (MessengerR gid) _ = do
      uid  <- requireAuthId
      auth <- runDB $ userInGroup uid gid
      return $ if auth then Authorized else Unauthorized ""
    isAuthorized         _        _ = isUser
    
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
    errorHandler  = customErrorW

-- | The page is available only if the user is logged in.
isUser :: Handler AuthResult
isUser =  AuthenticationRequired `maybe` const Authorized <$> maybeAuthId

userInGroup :: (MonadIO m) => UserId -> GroupId -> ReaderT SqlBackend m Bool
userInGroup userId groupId = isJust <$> getBy (UniqueGroups groupId userId)

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
    loginDest    = const HomeR
    
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

{- Request handling. -}

-- Template-based (DSL) route generator. Binds request handlers with routes.
mkYesodDispatch "FPTalks" $(parseRoutesFile "fptalks.routes")

-- | 'HomeR' (<localhost:3000/>) @GET@ request handler.
getHomeR :: Handler Html
getHomeR =  redirect ChatsR

getChatsR :: Handler Html
getChatsR =  do
  messages <- runDB . topMessages =<< requireAuthId
  defaultLayout $ do
    setTitle "FPTalks | Chats"
    [whamlet|
    <header>
      <h1>Chats
      ^{navigateW}
    <article>
      <ul>
        $forall Message{messageText, messageReceivers} <- messages
          <li.message>
            <a href=@{MessengerR messageReceivers}>#{messageText}
    |]

getSearchR :: Handler Html
getSearchR =  do
  thisUserId <- requireAuthId
  users <- runDB $ getUsers thisUserId
  (createW, encodeC) <- generateFormPost createChatForm
  defaultLayout $ do
    setTitle "FPTalks | User search"
    [whamlet|
    <header>
      <h1>Users
      ^{navigateW}
    <article>
      <ul>
        $forall Entity userId User{userEmail} <- users
          <li.user>
            <form action=@{CreateChatR} method=post encode=#{encodeC}>
              ^{createW userId}
              <input.link.user type=submit value=#{userEmail}>
    |]

--------------------------------------------------------------------------------

getMessengerR :: GroupId -> Handler Html
getMessengerR groupId = do
  messages <- runDB $ selectList [MessageReceivers ==. groupId] [Asc MessageId]
  (sendW, enctypeS) <- generateFormPost sendMessageForm
  defaultLayout $ do
    setTitle "FPTalks | Messenger"
    [whamlet|
    <header>
      ^{navigateW}
      <h1>Messenger
    <article>
      <ul.hidden>
        $forall Entity _ Message{messageText, messageSender} <- messages
          <li.message>
            <p.sender>#{messageSender}
            <p.message>#{messageText}
    <footer>
      <form#messageForm.message-box method=post enctype=#{enctypeS} action=@{MessengerR groupId}>
        ^{sendW}
        <input#messageFormSubmit type=submit>
    |]

postMessengerR :: GroupId -> Handler ()
postMessengerR groupId = do
  userId <- requireAuthId
  result <- fst . fst <$> runFormPost sendMessageForm
  case result of
    FormSuccess (Textarea text) -> void . runDB $ insert (Message text userId groupId)
    _                           -> return ()
  redirect (MessengerR groupId)

sendMessageForm :: Html -> MForm Handler (FormResult Textarea, Widget)
sendMessageForm =
  let settings = ""
        {
          fsName  = Just "MessageText",
          fsAttrs =
            [
              ("placeholder", "Write message..."),
              ("class", "message-box"),
              ("for", "messageForm")
            ]
        }
  in  renderDivs $ areq textareaField settings Nothing

--------------------------------------------------------------------------------

postCreateChatR :: Handler ()
postCreateChatR =  do
  result <- fst . fst <$> runFormPost createChatForm
  case result of
    FormSuccess interlocId -> redirect . MessengerR =<< requireDialog interlocId
    _                      -> redirect HomeR

createChatForm :: Html -> MForm Handler (FormResult UserId, UserId -> Widget)
createChatForm widget = do
  userId <- fst <$> mreq userField (""{fsName = Just "userId"}) Nothing
  return
    (
      userId,
      \ publicId -> [whamlet|
        <input type=number name=userId hidden value=#{publicId}>
        ^{widget}
      |]
    )

--------------------------------------------------------------------------------

instance RenderMessage FPTalks FormMessage
  where
    renderMessage _ _ = defaultFormMessage

--------------------------------------------------------------------------------

navigateW :: Widget
navigateW =  [whamlet|
<nav>
  <a href=@{ChatsR}>Chats
  <a href=@{SearchR}>Users
|]

--------------------------------------------------------------------------------

getUsers :: (MonadIO m) => UserId -> ReaderT SqlBackend m [Entity User]
getUsers userId =  filter ((/= userId) . entityKey) <$> selectList [] []

userGroups :: (MonadIO m) => UserId -> ReaderT SqlBackend m [GroupId]
userGroups userId = map (groupsGid . entityVal) <$> selectList [GroupsMember==.userId] []

topMessages :: (MonadIO m) => UserId -> ReaderT SqlBackend m [Message]
topMessages userId = map entityVal <$> do
  groups <- userGroups userId
  selectList [MessageReceivers<-.groups] [Desc MessageId, LimitTo (length groups)]

--------------------------------------------------------------------------------

requireDialog :: UserId -> Handler GroupId
requireDialog interlocId = do
  userId <- requireAuthId
  runDB $ do
    groups <- userGroups userId
    
    let addDialog = do
          gid <- insert (Group Nothing 2)
          void $ insert (Groups gid interlocId)
          void $ insert (Groups gid userId)
          return gid
    
    maybe addDialog (return . groupsGid . entityVal) . listToMaybe
      =<< selectList [GroupsMember==.interlocId, GroupsGid<-.groups] [LimitTo 1]



