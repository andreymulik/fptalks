{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

{- |
    Module      :  Sendmail
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "Sendmail" provides @sendmail@ functions that too big for "Foundation"
    module.
-}
module Sendmail where

import Yesod.Auth.Email
import Yesod.Core

-- Mail support
-- import Network.Mail.Mime hiding ( htmlPart )

-- Basics
-- import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text               (   unpack   )

-- import Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
-- import Text.Shakespeare.Text         (   stext    )

default ()

--------------------------------------------------------------------------------

sendEmail :: (MonadIO m) => Email -> VerUrl -> m ()
sendEmail email verurl = do
  -- Print out to the console the verification email, for easier debugging.
  liftIO $ putStrLn $ "Copy/Paste this URL in your browser:" ++ unpack verurl
  
  {-
    As planned, this piece of code should create the text of the letter and send
    it to the address specified by the user.
    
    However, you need to install and configure the @sendmail@ program (if it's
    absent in the PATH, the program will crash), as well and place the site on
    the hosting (there must be some domain name or at least a static ip in the
    site address property @approot@). In view of the possibility of the absence
    of the above on the checking computer, I commented out this part.
  -}
  
  -- Send email via the default @sendmail@ executable with default options.
  {-
  liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
    {
      mailTo      = [Address Nothing email],
      mailHeaders = [("Subject", "Verify your email address")],
      mailParts   =
        [[
          Part
          {
            partType        = "text/plain; charset=utf-8",
            partHeaders     = [],
            partEncoding    = None,
            partDisposition = DefaultDisposition,
            partContent     = PartContent $ encodeUtf8 [stext|
              Please confirm your email address by clicking on the link below.
              
              #{verurl}
              
              Thank you
            |]
          },
          Part
          {
            partType        = "text/html; charset=utf-8",
            partHeaders     = [],
            partEncoding    = None,
            partDisposition = DefaultDisposition,
            partContent     = PartContent $ renderHtml [shamlet|
              <p>Please confirm your email address by clicking on the link below.
              <p>
                <a href=#{verurl}>#{verurl}
              <p>Thank you
            |]
          }
        ]]
    }
  -}





