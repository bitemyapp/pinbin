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
import qualified Database.Esqueleto as E

-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Text.Julius (RawJS (..))

getUserR :: UserNameP -> Handler Html
getUserR uname =
  _getUser uname SharedAll (TagsP [])

getUserSharedR :: UserNameP -> SharedP -> Handler Html
getUserSharedR uname sharedp =
  _getUser uname sharedp (TagsP [])

getUserTagsR :: UserNameP -> TagsP -> Handler Html
getUserTagsR uname pathtags =
  _getUser uname SharedAll pathtags

_getUser :: UserNameP -> SharedP -> TagsP -> Handler Html
_getUser (UserNameP uname) sharedp (TagsP pathtags) = do
  (limit, page) <- _lookupPagingParams
  (bcount, bmarks, alltags) <-
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       (cnt, bm) <- bookmarksQuery userId sharedp pathtags limit page
       tg <- tagsQuery bm
       pure (cnt, bm, tg)
  defaultLayout $ do $(widgetFile "user")
  where
    _lookupPagingParams =
      (,)
      <$> fmap parseMaybe (lookupGetParam "count")
      <*> fmap parseMaybe (lookupGetParam "page")
      where
        parseMaybe x = readMaybe . unpack =<< x
