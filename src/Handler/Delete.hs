{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Delete where

import Database.Persist.Sql

import Import

postDeleteR :: Int64 -> Handler Html
postDeleteR bid = do
  let bid' = toSqlKey bid
  userId <- requireAuthId
  runDB $
    do bmark <- get404 bid'
       if bookmarkUserId bmark == userId
         then deleteCascade bid' >> pure ""
         else notFound
  
