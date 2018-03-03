{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Model
import ModelCrypto

import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P

import ClassyPrelude

import Options.Generic

data MigrationOpts
  = Migrate { conn :: Text}
  | Import { conn :: Text
           , userName :: Text
           , userPassword :: Text
           , userApiToken :: Maybe Text
           , bookmarkFile :: FilePath}
  deriving (Generic, Show)

instance ParseRecord MigrationOpts

main :: IO ()
main =
  getRecord "Migrations" >>= \case
    Migrate conn -> P.runSqlite conn runMigrations
    Import conn uname upass utoken file ->
      P.runSqlite conn $
      do hash <- liftIO $ hashPassword upass
         uid <- P.insert $ User uname hash utoken
         insertFileBookmarks uid file
