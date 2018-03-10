{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Edit where

import Database.Persist.Sql

import Import

postEditR :: Int64 -> Handler Html
postEditR bid = do
  userId <- requireAuthId
  let k_bid = toSqlKey bid
  runDB $ do
    bmark <- get404 k_bid >>= \case
       bmark' | bookmarkUserId bmark' == userId -> pure bmark'
       _ -> notFound
    lookupPostParam "action" >>= \case
      Just "delete" -> deleteCascade k_bid >> pure ""
      a -> invalidArgs (maybeToList a)
