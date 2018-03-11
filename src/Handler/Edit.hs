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
    res :: Maybe EditForm <- runPost
    lookupPostParam "action" >>= \case
      Just "delete" -> deleteCascade k_bid >> pure ""
      a -> invalidArgs (maybeToList a)

-- EditForm

data EditForm = EditForm
  { action :: Text
  } deriving (Show, Eq, Read, Generic)

instance FromJSON EditForm
instance MkAForm EditForm where
  mkAForm = mkEditAForm

mkEditAForm :: MonadHandlerForm m => AForm m EditForm
mkEditAForm = do
    EditForm
      <$> areq textField "action" Nothing
