{-# LANGUAGE OverloadedStrings #-}

{- |
    Module      :  Main
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "Main" provides simple server loader. @FPTalks@ uses @Warp@ server, writen
    specifically for @Yesod@.
-}
module Main where

import Foundation

import Yesod.Core

import Database.Persist.Sqlite

import Control.Monad.Logger ( runNoLoggingT )

default ()

--------------------------------------------------------------------------------

-- | Application entry point, runs the server on <localhost:3000>.
main :: IO ()
main =  runNoLoggingT $ "email.db3" `withSqliteConn` \ conn -> liftIO $ do
  runSqlConn (runMigration migrateAll) conn
  warp 3000 (FPTalks conn)




