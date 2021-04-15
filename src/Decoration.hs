{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}

{- |
    Module      :  Decoration
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "Decoration" provides decorated pages instead default layout, error page,
    etc.
-}
module Decoration
(
  -- * Layout
  customLayout,
  
  -- ** Pages
  customErrorW, customLoginW, customEmailLoginW, customRegisterW,
  customSetPasswordW
)
where

import Network.Wai ( rawPathInfo )

import Yesod.Form.Types
import Yesod.Form

import Yesod.Auth.Message
import Yesod.Auth.Email
import Yesod.Auth

import Yesod.Core

import Styles

import Data.ByteString.Char8 ( unpack )

import Data.Text.Encoding.Error (      lenientDecode      )
import Data.Text.Encoding       (      decodeUtf8With     )
import Data.Text                ( Text, pack, intercalate )

import Text.Blaze ( ToMarkup (..) )

import Control.Applicative

default ()

--------------------------------------------------------------------------------

-- | 'defaultLayout' variation with styles.
customLayout :: (Yesod site) => WidgetFor site () -> HandlerFor site Html
customLayout fptalksContentW = do
  page <- widgetToPageContent (do fptalksStyleW; fptalksContentW)
  withUrlRenderer [hamlet|
    $newline never
    $doctype 5
    <html>
      <head>
        <title>#{pageTitle page}
        ^{pageHead page}
      <body>
        ^{pageBody page}
  |]

--------------------------------------------------------------------------------

{- |
  The 'defaultErrorHandler' variation, responsible for the appearance of the
  pages that are displayed on some errors (missing page, internal server error,
  denied permission, etc.)
-}
customErrorW :: (Yesod site) => ErrorResponse -> HandlerFor site TypedContent
customErrorW NotFound = selectRep $ do
    provideRep . defaultLayout $ message' . content =<< waiRequest -- HTML page
    provideRep . return $ object ["message" .= notFound']          -- JSON value
    provideRep $ return notFound'                                  -- Plain text
  where
    -- Pretty error page
    message' = customMessageWidgetP "Page not found" "Please, go to other page."
    -- Pretty printed message
    content r = "Page " <> decodeUtf8With lenientDecode (rawPathInfo r) <>
      " is not found"
    -- Error text (JSON value and plain text)
    notFound' = pack "Not Found"

-- Must be unreachable
customErrorW NotAuthenticated = selectRep $ do
    provideRep $ message' content'
    provideRep $ do
      addHeader "WWW-Authenticate" "RedirectJSON realm=\"application\", \
        \param=\"authentication_url\""
      site <- getYesod
      rend <- getUrlRender
      let
          apair u = ["authentication_url" .= rend u]
          content = maybe [] apair (authRoute site)
      return . object $ ("message" .= notLogged) : content
    provideRep $ return notLogged
  where
    -- Pretty error page
    message' = defaultLayout . customMessageWidgetP "Not logged in"
      "Please, log in."
    -- Pretty printed message
    content'  = pack "Undeclared authRoute (internal problem)"
    -- Error text (JSON value and plain text)
    notLogged = pack "Not logged in"

customErrorW (PermissionDenied content) = selectRep $ do
    provideRep $ message' content
    provideRep . return $ object ["message" .= permDenied]
    provideRep $ return permDenied
  where
    -- Pretty error page
    message' = defaultLayout . customMessageWidgetP "Permission Denied"
      "Contact the site administrator."
    -- Error text (JSON value and plain text)
    permDenied = "Permission Denied. " <> content

customErrorW (InvalidArgs args) = selectRep $ do
    provideRep . defaultLayout $ message' [whamlet|
      <ul>
        $forall arg <- args
          <li>#{arg}
    |]
    provideRep . return $ object
      [
        "message" .= pack "Invalid Arguments",
        "errors"  .= args
      ]
    provideRep . return $ "Invalid Arguments: " <> intercalate " " args
  where
    message' = customMessageWidget "Invalid Arguments" "Check that the request \
      \is written correctly and that it matches the current API."

-- Must be unreachable
customErrorW (InternalError e) = do
    $logErrorS "yesod-core" e
    selectRep $ do
      provideRep . defaultLayout $ message' [whamlet|<pre.message-box>#{e}|]
      provideRep . return $ object
        [
          "message" .= pack "Internal Server Error",
          "error"   .= e
        ]
      provideRep . return $ "Internal Server Error: " <> e
  where
    message' = customMessageWidget "Internal Server Error" "It looks like \
      \someone forgot to handle the exception."

customErrorW (BadMethod m) = selectRep $ do
  provideRep . defaultLayout $ defaultMessageWidget "Method Not Supported"
    [hamlet|<p>Method <code>#{unpack m}</code> not supported|]
  provideRep . return $ object
    [
      "message" .= pack "Bad method",
      "method"  .= decodeUtf8With lenientDecode m
    ]
  provideRep . return $ "Bad Method " <> decodeUtf8With lenientDecode m

--------------------------------------------------------------------------------

{- |
  The 'defaultLoginHandler' variation, responsible for the appearance of the
  @\/auth*\/@ pages.
-}
customLoginW :: (YesodAuthEmail site) => AuthHandler site Html
customLoginW =  do
  authR <- getRouteToParent
  authLayout $ do
    let title = "Log In"
    setTitle title
    [whamlet|
      <header>
        <h1>#{title}
      <article>
        ^{apLogin authEmail authR}
    |]

{- |
  Default implementation of 'emailLoginHandler'.
  
  @since 1.4.17
-}
customEmailLoginW :: (YesodAuthEmail site) => (Route Auth -> Route site) -> WidgetFor site ()
customEmailLoginW toParent = do
    (widget, enctype) <- generateFormPost loginForm
    [whamlet|
      <div#emailLoginForm.auth>
        <form method="post" action="@{toParent loginR}" enctype=#{enctype}>
          ^{widget}
          <div>
            <button.btn type=submit>
              _{LoginViaEmail}
            <a.btn href="@{toParent registerR}">
              _{RegisterLong}
    |]
  where
    loginForm extra = do
      emailMsg <- renderMessage' Email
      (emailRes, emailView) <- mreq emailField (emailSettings emailMsg) Nothing
      
      passwordMsg <- renderMessage' Password
      (passwordRes, passwordView) <- mreq passwordField (passwordSettings passwordMsg) Nothing
      
      let
        userRes = liftA2 UserLoginForm emailRes passwordRes
        widget  = [whamlet|
          #{extra}
          ^{fvInput emailView}
          ^{fvInput passwordView}
        |]
      
      return (userRes, widget)
    
    settings' name label attrs = FieldSettings
      {
        fsAttrs = attrs, fsId = Just name, fsName = Just name,
        fsLabel = SomeMessage label, fsTooltip = Nothing
      }
    
    passwordSettings msg = settings' "password" Password [("placeholder", msg)]
    emailSettings    msg = settings' "email" Email
      [("autofocus", ""), ("placeholder", msg)]
    
    renderMessage' msg = do
      master <- getYesod
      langs  <- languages
      return $ renderAuthMessage master langs msg

customRegisterW :: AuthHandler site Html
customRegisterW =  do
    (widget, enctype) <- generateFormPost registrationForm
    toParentRoute     <- getRouteToParent
    authLayout $ do
      setTitle "Register"
      [whamlet|
        <header>
          <h1>Register
        <article>
          <div#registerForm.auth>
            <form method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
              ^{widget}
              <div>
                <button.btn>_{Register}
      |]
  where
    registrationForm extra = do
        let
          emailSettings = FieldSettings
            {
              fsId      = Just "email",
              fsName    = Just "email",
              fsLabel   = SomeMessage Email,
              fsAttrs   = [("autofocus", ""), ("placeholder", "Register a new account")],
              fsTooltip = Nothing
            }
        
        (emailRes, emailView) <- mreq emailField emailSettings Nothing
        
        let
          userRes = UserForm <$> emailRes
          widget  = [whamlet|
            #{extra}
            ^{fvInput emailView}
          |]
        return (userRes, widget)

customSetPasswordW :: YesodAuthEmail master => Bool -> AuthHandler master TypedContent
customSetPasswordW needOld = do
    messageRender <- getMessageRender
    toParent      <- getRouteToParent
    selectRep $ do
      provideJsonMessage (messageRender SetPass)
      provideRep . authLayout $ do
        (widget, enctype) <- generateFormPost setPasswordForm
        setTitleI SetPass
        [whamlet|
          <header>
            <h1>_{SetPass}
          <article>
            <div#setPasswordForm.auth>
              <form method="post" action="@{toParent setpassR}" enctype=#{enctype}>
                ^{widget}
        |]
  where
    setPasswordForm extra = do
        (curPasRes, curPasView) <- mreq passwordField curPasSettings Nothing
        (newPasRes, newPasView) <- mreq passwordField newPasSettings Nothing
        (conPasRes, conPasView) <- mreq passwordField conPasSettings Nothing
        
        let
          passwordFormRes = liftA3 PasswordForm curPasRes newPasRes conPasRes
          widget          = [whamlet|
            #{extra}
              $if needOld
                ^{fvInput curPasView}
              ^{fvInput newPasView}
              ^{fvInput conPasView}
              <input type=submit value=_{SetPassTitle}>
          |]
        return (passwordFormRes, widget)
    
    settings' name placeholder msg attrs = FieldSettings
      {
        fsLabel = SomeMessage msg, fsName = Just name, fsTooltip = Nothing,
        fsAttrs = ("autofocus", "") : ("placeholder", placeholder) : attrs, fsId = Just (name <> "Password")
      }
    
    curPasSettings = settings' "current" "Current password" ConfirmPass     []
    conPasSettings = settings' "confirm" "Confirm password" CurrentPassword []
    newPasSettings = settings'   "new"       "New password" CurrentPassword
      [(":not", ""), ("needOld:autofocus", "")]

data UserLoginForm      = UserLoginForm Text Text
data PasswordForm       = PasswordForm  Text Text Text
data UserForm           = UserForm      Text

--------------------------------------------------------------------------------

customMessageWidgetP :: (Yesod site, ToMarkup content) => Html -> Text -> content -> WidgetFor site ()
customMessageWidgetP title message content = customMessageWidget title message
  [whamlet|<p.message-box>#{content}|]

customMessageWidget :: (Yesod site) => Html -> Text -> WidgetFor site () -> WidgetFor site ()
customMessageWidget title message content = do
  setTitle title
  [whamlet|
    <header>
      <h1>#{title}
      <p>#{message}
    <article>
      ^{content}
  |]




