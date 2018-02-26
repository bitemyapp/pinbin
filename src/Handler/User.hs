{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.User where

import Import
import Text.Read
import Database.Persist.Sql

-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Text.Julius (RawJS (..))

getUserR :: PathUserName -> Handler Html
getUserR (PathUserName uname) = do
  count' <- lookupGetParam "count"
  page <- lookupGetParam "page"
  bmarks <- runDB $ do
    Entity userId _ <- getBy404 $ UniqueUserName uname
    bookmarksByDate userId
      (count' >>= readMaybe . unpack)
      (page >>= readMaybe . unpack)
    >>= withTags
  defaultLayout $ do $(widgetFile "user")

getUserTagsR :: PathUserName -> PathTags -> Handler Html
getUserTagsR (PathUserName uname) (PathTags tags) = do
  count' <- lookupGetParam "count"
  page <- lookupGetParam "page"
  bmarks <- runDB $ do
    Entity userId _ <- getBy404 $ UniqueUserName uname
    bookmarksByTags userId tags
      (count' >>= readMaybe . unpack)
      (page >>= readMaybe . unpack)
    >>= withTags
  defaultLayout $ do $(widgetFile "user")
