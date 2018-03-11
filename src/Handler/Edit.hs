{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Edit where

import Database.Persist.Sql

import Import

deleteDeleteR :: Int64 -> Handler Html
deleteDeleteR bid = do
  userId <- requireAuthId
  runDB $ do
    let k_bid = toSqlKey bid
    void $ requireResource userId k_bid
    deleteCascade k_bid
  pure ""

postReadR :: Int64 -> Handler Html
postReadR bid = do
  userId <- requireAuthId
  runDB $ do
    let k_bid = toSqlKey bid
    bm <- requireResource userId k_bid
    update k_bid [BookmarkToRead =. False]
  pure ""

-- common

requireResource :: UserId -> Key Bookmark -> DBM Handler Bookmark
requireResource userId k_bid =
  get404 k_bid >>= \case
    bmark | bookmarkUserId bmark == userId -> pure bmark
    _ -> notFound

-- EditForm

data EditForm = EditForm {} deriving (Show, Eq, Read, Generic)

instance FromJSON EditForm

mkEditIForm
  :: MonadHandlerForm m
  => FormInput m EditForm
mkEditIForm = do
  pure EditForm
