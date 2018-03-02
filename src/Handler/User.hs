{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.User where

import Import
import Text.Read
import Database.Persist.Sql
import qualified Database.Esqueleto as E

-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Text.Julius (RawJS (..))

getUserR :: UserNameP -> Handler Html
getUserR uname =
  _getUser uname SharedAll FilterAll (TagsP [])

getUserSharedR :: UserNameP -> SharedP -> Handler Html
getUserSharedR uname sharedp =
  _getUser uname sharedp FilterAll (TagsP [])

getUserFilterR :: UserNameP -> FilterP -> Handler Html
getUserFilterR uname sharedp =
  _getUser uname SharedAll sharedp (TagsP [])

getUserTagsR :: UserNameP -> TagsP -> Handler Html
getUserTagsR uname pathtags =
  _getUser uname SharedAll FilterAll pathtags

_getUser :: UserNameP -> SharedP -> FilterP -> TagsP -> Handler Html
_getUser (UserNameP uname) sharedp filterp (TagsP pathtags) = do
  (limit', page') <- _lookupPagingParams
  let limit = maybe 100 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
  (bcount, bmarks, alltags) <-
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       (cnt, bm) <- bookmarksQuery userId sharedp filterp pathtags limit page
       tg <- tagsQuery bm
       pure (cnt, bm, tg)
  mroute <- getCurrentRoute 
  let pager = $(widgetFile "pager")
  defaultLayout $ do
    $(widgetFile "user")

_lookupPagingParams :: Handler (Maybe Int64, Maybe Int64)
_lookupPagingParams =
    (,)
    <$> fmap parseMaybe (lookupGetParam "count")
    <*> fmap parseMaybe (lookupGetParam "page")
    where
    parseMaybe x = readMaybe . unpack =<< x

