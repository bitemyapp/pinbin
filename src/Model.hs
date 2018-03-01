{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import qualified Data.Aeson as A
import qualified Pinboard as P

import Control.Monad.Writer hiding ((<>))

import Types
import ClassyPrelude.Yesod

import Database.Persist.Sql
import Database.Esqueleto hiding ((==.))
import qualified Database.Esqueleto as E


share [mkPersist sqlSettings, mkMigrate "migrateSchema"] [persistLowerCase| 
User
  name Text
  UniqueUserName name
  deriving Show Eq Typeable Ord

Bookmark
  userId UserId
  href Text
  description Text
  extended Text
  time UTCTime
  shared Bool
  toRead Bool
  selected Bool
  UniqueUserHref userId href
  deriving Show Eq Typeable Ord

BookmarkTag
  userId UserId
  tag Text
  bookmarkId BookmarkId
  seq Int
  UniqueUserTagBookmarkId userId tag bookmarkId
  UniqueUserBookmarkIdTagSeq userId bookmarkId tag seq
  deriving Show Eq Typeable Ord
|]

-- Migration

toMigration :: [Text] -> Migration
toMigration = lift . tell . fmap (False ,)

migrateIndexes :: Migration
migrateIndexes =
  toMigration
    [ "CREATE INDEX IF NOT EXISTS idx_bookmark_time ON bookmark (user_id, time DESC)"
    , "CREATE INDEX IF NOT EXISTS idx_bookmark_tag_bookmark_id ON bookmark_tag (bookmark_id)"
    ]


-- DB

_sumValues :: [E.Value Int] -> Int
_sumValues v = sum $ fmap E.unValue v

getUserByName :: Text -> DB (Maybe (Entity User))
getUserByName uname =
  return . headMay =<<
  (select $
   from $ \u -> do
   where_ (u ^. UserName E.==. val uname)
   pure u)

bookmarksByDate :: Key User -> Maybe Int -> Maybe Int -> DB (Int, [Entity Bookmark])
bookmarksByDate userId count' page =
    (,) -- total count
    <$> fmap _sumValues
        (select $
        from $ \b -> do
        _inner b
        pure $ E.val 1)
        -- paged data
    <*> (select $
         from $ \b -> do
         _inner b
         orderBy [desc (b ^. BookmarkTime)]
         limit count''
         offset ((page' - 1) * count'')
         pure b)
  where
    _inner b =
      where_ (b ^. BookmarkUserId E.==. val userId)
    count'' = maybe 100 fromIntegral count'
    page' = maybe 1 fromIntegral page


bookmarksByTags :: Key User
                -> [P.Tag]
                -> Maybe Int
                -> Maybe Int
                -> DB (Int, [Entity Bookmark])
bookmarksByTags userId tags count' page =
    (,) -- total count
    <$> fmap _sumValues
        (select $
        from $ \(b, t) -> do
        _inner b t
        pure $ E.val 1)
        -- paged data
    <*> (select $
         from $ \(b, t) -> do
         _inner b t
         orderBy [desc (b ^. BookmarkTime)]
         limit count''
         offset ((page' - 1) * count'')
         pure b)
  where
    _inner b t = do
      where_  ( b ^. BookmarkUserId E.==. val userId
            &&. b ^. BookmarkId E.==. t ^. BookmarkTagBookmarkId
            &&. t ^. BookmarkTagTag `in_` valList tags )
      E.groupBy (b ^. BookmarkId)
      having (E.count (b ^. BookmarkId) E.==. val (length tags))
    count'' = maybe 100 fromIntegral count'
    page' = maybe 1 fromIntegral page

withTags :: [Entity Bookmark] -> DB [Entity BookmarkTag]
withTags bmarks =
  select $
  from $ \t -> do
  where_ (t ^. BookmarkTagBookmarkId `in_` valList (fmap entityKey bmarks))
  orderBy [asc (t ^. BookmarkTagSeq)]
  pure t

bookmarkEntityToTags :: Entity Bookmark -> [P.Tag] -> [BookmarkTag]
bookmarkEntityToTags (Entity {entityKey = bookmarkId
                             ,entityVal = Bookmark {..}}) tags =
  fmap
    (\(i, tag) -> BookmarkTag bookmarkUserId tag bookmarkId i)
    (zip [1 ..] tags)

postToBookmark :: UserId -> P.Post -> Bookmark
postToBookmark user P.Post {..} =
  Bookmark user postHref postDescription postExtended postTime postShared postToRead False

-- Bookmark Files

insertFileBookmarks :: MonadLogger m => Key User -> FilePath -> DBM m ()
insertFileBookmarks userId bookmarkFile = do
  $logDebug $ "Reading bookmark file: " <> pack bookmarkFile
  posts' <- liftIO $ readBookmarkFileJson bookmarkFile
  case posts' of
      Left e -> $logError $ pack e
      Right posts -> do
        $logDebug $ (pack . show . length) posts <> " bookmarks read"
        void $ do
            let bookmarks = fmap (postToBookmark userId) posts
            bookmarkIds <- insertMany bookmarks
            insertMany_ $ concatMap (uncurry bookmarkEntityToTags)
                (zipWith3 (\k v p -> (Entity k v, P.postTags p)) bookmarkIds bookmarks posts)
  where
    readBookmarkFileJson :: MonadIO m => FilePath -> m (Either String [P.Post])
    readBookmarkFileJson fpath = pure . A.eitherDecode' . fromStrict =<< readFile fpath
