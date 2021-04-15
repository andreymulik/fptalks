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
  module Yesod.Static,
  module Yesod.Core,
  
  -- * Web
  FPTalks (..), Handler, Widget, resourcesFPTalks, migrateAll
)
where

-- Web
import Yesod.Persist.Core

import Yesod.Auth.Email
import Yesod.Auth

import Yesod.Static
import Yesod.Form

import Yesod.Core.Types ( FileInfo (..) )
import Yesod.Core       hiding ( addMessage )

import Decoration
import Sendmail

-- Database
import Database.Persist.Sqlite
import Database.Persist.TH

-- Basics
import Data.Bifunctor
import Data.Typeable  ( Typeable )
import Data.Coerce
import Data.Maybe
import Data.Text ( Text, strip )
import Data.List ( nub )
import Data.Int

import Text.Blaze ( ToMarkup (..) )

import Control.Monad.Trans.Reader
import Control.Monad ( void, join, when )

import Control.Applicative

import System.Directory ( doesPathExist )
import System.FilePath  ( (</>) )

import Web.ClientSession ( defaultKeyFile )

default ()

--------------------------------------------------------------------------------

{- Database stuff -}

-- Composite DSL: Haskell-side types, selectors and migrations declared here
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name       Text
  icon       FilePath
  email      Text
  password   Text Maybe
  verkey     Text Maybe
  verified   Bool
  UniqueUser email
  deriving   Typeable

Dialog
  group GroupId
  user1 UserId
  user2 UserId
  UniqueDialog user1 user2
  deriving Typeable

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
  sender    Text
  senderId  UserId
  receivers GroupId
  deriving  Typeable
|]

--------------------------------------------------------------------------------

{- Display DB keys in Html (Markup) DSL. -}

instance ToMarkup UserId    where toMarkup = toMarkup . fromUserId
instance ToMarkup GroupId   where toMarkup = toMarkup . fromGroupId
instance ToMarkup MessageId where toMarkup = toMarkup . fromMessageId

--------------------------------------------------------------------------------

{- Convert DB keys to and from Int's -}

fromUserId :: UserId -> Int64
fromUserId =  coerce

toUserId :: Int64 -> UserId
toUserId =  coerce

toMessageId :: Int64 -> MessageId
toMessageId =  coerce

fromMessageId :: MessageId -> Int64
fromMessageId =  coerce

toGroupId :: Int64 -> GroupId
toGroupId =  coerce

fromGroupId :: GroupId -> Int64
fromGroupId =  coerce

--------------------------------------------------------------------------------

{- Application state representation. -}

{- |
  'FPTalks' is main site type, that represent whole web application. 'FPTalks'
  value contains @SQL@ connection representation.
-}
data FPTalks = FPTalks {sqlBackend :: SqlBackend, getStatic :: Static}

{-
  Template-based (DSL) route generator. Creates data type 'Route' 'FPTalks',
  which represents site routes.
-}
mkYesodData "FPTalks" $(parseRoutesFile "fptalks.routes")

--------------------------------------------------------------------------------

{- Basic web application behavior. -}

instance Yesod FPTalks
  where
    -- | Page access control.
    isAuthorized (DelMessageR gid) _ = isGroupUser gid
    isAuthorized (MessengerR  gid) _ = isGroupUser gid
    isAuthorized (StaticR       _) _ = isPublic
    isAuthorized (IconR         _) _ = isPublic
    isAuthorized (AuthR         _) _ = isPublic
    isAuthorized         _         _ = isUser
    
    makeSessionBackend _ = strictSameSiteSessions defaultSessionBackend
    
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

instance RenderMessage FPTalks FormMessage
  where
    renderMessage _ _ = defaultFormMessage

isPublic :: Handler AuthResult
isPublic =  return Authorized

-- | The page is available only if the user is logged in.
isUser :: Handler AuthResult
isUser =  AuthenticationRequired `maybe` const Authorized <$> maybeAuthId

isGroupUser :: GroupId -> Handler AuthResult
isGroupUser groupId =  do
  userId <- requireAuthId
  auth <- runDB $ userInGroup userId groupId
  return $ if auth then Authorized else Unauthorized ""

userInGroup :: (MonadIO m) => UserId -> GroupId -> ReaderT SqlBackend m Bool
userInGroup userId groupId = isJust <$> getBy (UniqueGroups groupId userId)

defaultSessionBackend :: IO (Maybe SessionBackend)
defaultSessionBackend =  Just <$> defaultClientSessionBackend 120 defaultKeyFile

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
    
    -- Run some action in 'FPTalks' @SQL@ backend from HTTP request handler.
    runDB action = getYesod >>= runSqlConn action . sqlBackend

{- |
  The connecting link between the database and the authentication service. Adds
  one important utility function, extends 'YesodPersist'.
-}
instance YesodAuthPersist FPTalks

--------------------------------------------------------------------------------

{- DB queries. -}

-- | Get list of users.
getUsers :: (MonadIO m) => UserId -> ReaderT SqlBackend m [Entity User]
getUsers userId =  filter ((/= userId) . entityKey) <$> selectList [] []

-- | Get groups of given user.
userGroups :: (MonadIO m) => UserId -> ReaderT SqlBackend m [GroupId]
userGroups userId = map (groupsGid . entityVal) <$> selectList [GroupsMember==.userId] []

-- | Get last messages for each user groups.
topMessages :: (MonadIO m) => UserId -> ReaderT SqlBackend m [Message]
topMessages userId = map entityVal <$> do
  groups <- userGroups userId
  selectList [MessageReceivers<-.groups] [Desc MessageId, LimitTo (length groups)]

-- | Delete group from current user group list (hide and keep all messages).
delGroup :: GroupId -> Handler ()
delGroup groupId =
  let delete' = return () `maybe` (delete . entityKey)
  in  runDB . (delete' =<<) . getBy . UniqueGroups groupId =<< requireAuthId

setName :: (MonadIO m) => Key User -> Text -> ReaderT SqlBackend m ()
setName userId name = update userId [UserName =. strip name]

-- | Get all contacts for the given user.
getContacts :: (MonadIO m) => UserId -> ReaderT SqlBackend m [Entity User]
getContacts userId = do
  groups  <- userGroups userId
  members <- selectList [GroupsGid<-.groups, GroupsMember!=.userId] [Asc GroupsMember]
  sequence [Entity uid <$> getJust uid | uid <- nub (groupsMember . entityVal <$> members)]

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
    
    loginHandler = customLoginW -- work as default, just re-design
    authPlugins  = const [authEmail]
    logoutDest   = const (AuthR LoginR)
    loginDest    = const HomeR
    
    authenticate Creds{credsIdent = email} =
      let user = User email "/static/anon.png" email Nothing Nothing False
      in  liftHandler . runDB $ Authenticated . either entityKey id <$> insertBy user

{- |
  Authentication plugin for 'YesodAuth' service, defines some pages in
  @\/auth\/*@ subsite for @JSON@-based message exchange.
-}
instance YesodAuthEmail FPTalks
  where
    type AuthEmailId FPTalks = UserId
    
    registerHandler    = customRegisterW    -- work as default, just re-design
    emailLoginHandler  = customEmailLoginW  -- work as default, just re-design
    setPasswordHandler = customSetPasswordW -- work as default, just re-design
    afterPasswordRoute = logoutDest
    
    addUnverified email verkey = liftHandler . runDB . insert $
      User email "/static/anon.png" email Nothing (Just verkey) False
    
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

{- HTTP GET request handling. -}

-- Template-based (DSL) route generator. Binds request handlers with routes.
mkYesodDispatch "FPTalks" $(parseRoutesFile "fptalks.routes")

-- | 'HomeR' (<localhost:3000/>) @GET@ request handler.
getHomeR :: Handler Html
getHomeR =  redirect . UserR =<< requireAuthId

-- | 'UserR' (<localhost:3000/user/UserId>) @GET@ request handler.
getUserR :: UserId -> Handler Html
getUserR userId = do
  thisId   <- requireAuthId
  contacts <- runDB $ getContacts thisId
  User{..} <- runDB (getJust userId)
  (createW, encodeC) <- generateFormPost createChatForm
  (nameW,  enctypeN) <- generateFormPost (nameForm userName)
  (iconW,  enctypeI) <- generateFormPost iconForm
  defaultLayout $ do
    setTitle ("FPTalks | " <> toMarkup userName)
    [whamlet|
    <header>
      <h1>#{userName}
      ^{navigateW}
    <article>
      <div#accountBox>
        $if thisId == userId
          <img#userIcon src=#{userIcon} alt=#{userName} for=userIcon>
          <form#userName method=post action=@{UserNameR} enctype=#{enctypeN}>
            ^{nameW}
          <form#userIconForm method=post action=@{UserIconR} enctype=#{enctypeI}>
            ^{iconW}
            <noscript>
              <input type=submit value="Submit">
        $else
          <img.user-icon src=#{userIcon} alt=#{userName}>
          <p>#{userName}
          <form action=@{CreateChatR} method=post encode=#{encodeC}>
            ^{createW userId}
            <input.link.user type=submit value="Dialog with">
    |]

-- | 'ChatsR' (<localhost:3000/chat>) @GET@ request handler.
getChatsR :: Handler Html
getChatsR =  do
  (delW, enctypeD) <- generateFormPost delGroupForm
  messages <- runDB . topMessages =<< requireAuthId
  defaultLayout $ do
    setTitle "FPTalks | Chats"
    [whamlet|
    <header>
      <h1>Chats
      ^{navigateW}
    <article>
      <ul>
        $forall Message{..} <- messages
          <li.message>
            <form action=@{DelGroupR} method=post enctype=#{enctypeD}>
              ^{delW messageReceivers}
              <input.trash type=submit value="">
            <div.column>
              <a.user.small href=@{UserR messageSenderId}>#{messageSender}
              <a href=@{MessengerR messageReceivers}>#{messageText}
    |]

-- | 'UsersR' (<localhost:3000/user/list>) @GET@ request handler.
getUsersR :: Handler Html
getUsersR =  do
  users <- runDB . getUsers =<< requireAuthId
  (createW, encodeC) <- generateFormPost createChatForm
  defaultLayout $ do
    setTitle "FPTalks | User search"
    [whamlet|
    <header>
      <h1>Users
      ^{navigateW}
    <article>
      <ul>
        $forall Entity userId User{userName} <- users
          <li.user>
            <form action=@{CreateChatR} method=post encode=#{encodeC}>
              ^{createW userId}
              <input.link.user type=submit value=#{userName}>
    |]

-- | 'MessengerR' (<localhost:3000/chat/GroupId>) @GET@ request handler.
getMessengerR :: GroupId -> Handler Html
getMessengerR groupId = do
  messages <- runDB $ selectList [MessageReceivers ==. groupId] []
  userId   <- requireAuthId
  (sendW, enctypeS) <- generateFormPost sendMessageForm
  (delW,  enctypeD) <- generateFormPost delMessageForm
  
  defaultLayout $ do
    setTitle "FPTalks | Messenger"
    -- toWidgetHead [hamlet|<meta http-equiv="refresh" content="60">|]
    [whamlet|
    <header>
      <h1>Messenger
      ^{navigateW}
    <article>
      <ul.hidden>
        $forall Entity messageId Message{messageText, messageSender, messageSenderId} <- messages
          <li.message>
            $if messageSenderId == userId
              <form action=@{DelMessageR groupId} method=post enctype=#{enctypeD}>
                ^{delW messageId}
                <input.trash type=submit value="">
            <div.column>
              <a.user.small href=@{UserR messageSenderId}>#{messageSender}
              <p.message>#{messageText}
    <footer>
      <form#messageForm.message-box method=post action=@{MessengerR groupId} enctype=#{enctypeS}>
        ^{sendW}
        <input#messageFormSubmit type=submit value="Отправить">
    |]

-- | 'IconR' (<localhost:3000/user/icons/UserId>) @GET@ request handler.
getIconR :: UserId -> Handler ()
getIconR userId = do
  let path = "icons" </> show (fromUserId userId)
  ex <- liftIO $ doesPathExist path
  sendFile typePng (if ex then path else "static/anon.png")

--------------------------------------------------------------------------------

{- HTTP POST request handling. -}

-- | 'MessengerR' (<localhost:3000/chat/GroupId>) @POST@ request handler.
postMessengerR :: GroupId -> Handler ()
postMessengerR groupId = do
  userId <- requireAuthId
  name   <- userName . fromJust <$> runDB (get userId)
  result <- fst . fst <$> runFormPost sendMessageForm
  case result of
    FormSuccess (Textarea text) -> void . runDB $ insert (Message text name userId groupId)
    _                           -> return ()
  redirect (MessengerR groupId)

-- | 'CreateChatR' (<localhost:3000/chat/new>) @POST@ request handler.
postCreateChatR :: Handler ()
postCreateChatR =  do
  result <- fst . fst <$> runFormPost createChatForm
  case result of
    FormSuccess interlocId -> redirect . MessengerR =<< requireDialog interlocId
    _                      -> redirect HomeR

-- | 'DelMessageR' (<localhost:3000/chat/GroupId/delete>) @POST@ request handler.
postDelMessageR :: GroupId -> Handler ()
postDelMessageR groupId = do
  result <- fst . fst <$> runFormPost delMessageForm
  case result of
    FormSuccess messageId -> runDB $ do
      message <- get messageId
      user    <- get =<< requireAuthId
      case liftA2 (\ User{..} Message{..} -> userName == messageSender) user message of
        -- user exists, message exists and user is message sender
        Just True -> delete messageId
        _         -> return ()
    _             -> return ()
  redirect (MessengerR groupId)

-- | 'DelGroupR' (<localhost:3000/chat/leave>) @POST@ request handler.
postDelGroupR :: Handler ()
postDelGroupR =  do
  result <- fst . fst <$> runFormPost delGroupForm
  case result of {FormSuccess groupId -> delGroup groupId; _ -> return ()}
  redirect ChatsR

-- | 'UserNameR' (<localhost:3000/user/name>) @POST@ request handler.
postUserNameR :: Handler ()
postUserNameR =  do
  userId   <- requireAuthId
  User{..} <- runDB (getJust userId)
  result <- fst . fst <$> runFormPost (nameForm userName)
  case result of {FormSuccess name -> runDB $ setName userId name; _ -> return ()}
  redirect (UserR userId)

-- | 'UserIconR' (<localhost:3000/user/icon>) @POST@ request handler.
postUserIconR :: Handler ()
postUserIconR =  do
  userId <- requireAuthId
  result <- fst . fst <$> runFormPost iconForm
  case result of
    FormSuccess FileInfo{..} -> do
      let path = "icons" </> show (fromUserId userId)
      liftIO $ fileMove path
      runDB  $ update userId [UserIcon =. path]
    _                        -> return ()
  redirect (UserR userId)

--------------------------------------------------------------------------------

{- Html forms. -}

{- |
  Simple one-to-many form: widget (hidden input field) contain given 'UserId',
  but form handler accept any 'UserId' (form result).
-}
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

-- | Remove message form.
delMessageForm :: Html -> MForm Handler (FormResult MessageId, MessageId -> Widget)
delMessageForm widget = do
  result <- fst <$> mreq messageField ("" {fsName = Just "messageId"}) Nothing
  return
    (
      result,
      \ publicId -> [whamlet|
        <input type=number name=messageId required hidden value=#{publicId}>
        ^{widget}
      |]
    )

nameForm :: Text -> Html -> MForm Handler (FormResult Text, Widget)
nameForm name widget = second ((>> toWidget widget) . fvInput) <$>
  mreq textField (""{fsAttrs = [("placeholder", name), ("class", "name")]}) Nothing

iconForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
iconForm widget = do
  (result, view) <- mreq fileField (""{fsName = Just "fileName"}) Nothing
  return
    (
      result,
      [whamlet|
      <label for=#{fvId view}>Change avatar
        <input##{fvId view} name="fileName" type="file" required hidden onchange="this.form.submit()" accept="image/x-png">
      ^{widget}
      |]
    )

-- | Hide group for current user.
delGroupForm :: Html -> MForm Handler (FormResult GroupId, GroupId -> Widget)
delGroupForm widget = do
  result <- fst <$> mreq groupField ("" {fsName = Just "groupId"}) Nothing
  return
    (
      result,
      \ publicId -> [whamlet|
        <input type=number name=groupId required hidden value=#{publicId}>
        ^{widget}
      |]
    )

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

{- Form fields for DB keys. -}

userField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m UserId
userField =  convertField toUserId fromUserId intField

groupField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m GroupId
groupField =  convertField toGroupId fromGroupId intField

messageField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m MessageId
messageField =  convertField toMessageId fromMessageId intField

--------------------------------------------------------------------------------

navigateW :: Widget
navigateW =  [whamlet|
<nav>
  <a href=@{HomeR}>Account
  <a href=@{ChatsR}>Chats
  <a href=@{UsersR}>Users
|]

--------------------------------------------------------------------------------

requireDialog :: UserId -> Handler GroupId
requireDialog interlocId = do
  userId <- requireAuthId
  when (userId == interlocId) $ redirect (UserR userId)
  runDB $ do
    let
      addDialog = do
        -- Add users to group and user count to group info
        gid <- insert (Group Nothing 2)
        void $ insert (Dialog gid interlocId userId)
        void $ insert (Dialog gid userId interlocId)
        void $ insert (Groups gid interlocId)
        void $ insert (Groups gid userId)
        return gid
    
    maybe addDialog (return . dialogGroup . entityVal)
      =<< getBy (UniqueDialog userId interlocId)


