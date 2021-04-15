{-# LANGUAGE OverloadedStrings #-}

import Foundation

import Database.Persist.Sqlite

import Control.Monad.Logger ( runNoLoggingT )

default ()

--------------------------------------------------------------------------------

-- | Application entry point, runs the server on <localhost:3000>.
main :: IO ()
main =  runNoLoggingT $ "sqlite.db3" `withSqliteConn` \ sql -> liftIO $ do
  statics <- static "static"
  runSqlConn (runMigration migrateAll) sql
  warp 3000 (FPTalks sql statics)


