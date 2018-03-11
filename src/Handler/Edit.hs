{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Edit where

import Database.Persist.Sql

import Import

deleteEditR :: Int64 -> Handler Html
deleteEditR bid = do
  userId <- requireAuthId
  runDB $ do
    let k_bid = toSqlKey bid
    void $ requireResource userId k_bid
    deleteCascade k_bid
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
