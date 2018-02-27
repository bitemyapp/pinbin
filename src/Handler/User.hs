{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.User where

import Import
import Text.Read
import Database.Persist.Sql

-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Text.Julius (RawJS (..))

getUserR :: UserNameP -> Handler Html
getUserR (UserNameP uname) = do
  (count', page) <- _lookupPagingParams
  bmarks <-
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       withTags =<< bookmarksByDate userId count' page
  defaultLayout $ do $(widgetFile "user")

getUserTagsR :: UserNameP -> TagsP -> Handler Html
getUserTagsR (UserNameP uname) (TagsP tags) = do
  (count', page) <- _lookupPagingParams
  bmarks <-
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       withTags =<< bookmarksByTags userId tags count' page
  defaultLayout $ do $(widgetFile "user")

_lookupPagingParams :: Handler (Maybe Int, Maybe Int)
_lookupPagingParams =
  (,)
  <$> fmap parseMaybe (lookupGetParam "count")
  <*> fmap parseMaybe (lookupGetParam "page")
  where
    parseMaybe x = readMaybe . unpack =<< x
