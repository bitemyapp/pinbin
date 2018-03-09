
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Add where

import Import

getAddR :: Handler Html
getAddR = do
  userId <- requireAuthId

  murl <- lookupGetParam "url"
  mexisting <- runDB $ runMaybeT $ do
    bmark <- MaybeT . getBy . UniqueUserHref userId =<< (MaybeT $ pure murl)
    btags <- MaybeT $ Just <$> withTags (entityKey bmark)
    pure (bmark, btags)

  mgetdefs <- aFormToMaybeGetSuccess (mkAddAForm Nothing)

  (formWidget, _) <- generateFormPost $ renderTable $
    mkAddAForm (maybe mgetdefs (Just . toAddDefs) mexisting)

  viewAddWidget
    formWidget
    (mexisting $> $(widgetFile "add-exists-alert"))
    (maybe "url" (const "tags") murl :: Text)
  where
    toAddDefs :: (Entity Bookmark, [Entity BookmarkTag]) -> AddForm
    toAddDefs (Entity _ Bookmark {..}, tags) =
      AddForm
      { url = bookmarkHref
      , title = Just bookmarkDescription
      , description = Just $ Textarea $ bookmarkExtended
      , tags = Just $ unwords $ fmap (bookmarkTagTag . entityVal) tags
      , private = Just $ not bookmarkShared
      , toread = Just $ bookmarkToRead
      }

postAddR :: Handler Html
postAddR = do
  userId <- requireAuthId

  ((formResult, formWidget), _) <- runFormPost $ renderTable $ mkAddAForm Nothing

  case formResult of
    FormSuccess addForm -> do
      time <- liftIO getCurrentTime
      void $ runDB $ upsertDB
        (toBookmark userId time addForm)
        (maybe [] words (tags addForm))
      lookupGetParam "next" >>= \case
        Just next -> redirect next
        Nothing -> popupLayout Nothing [whamlet| <div .alert> Add Successful </div> <script> window.close() </script> |]
    _ ->
      viewAddWidget formWidget Nothing "Tags"

  where
    toBookmark :: UserId -> UTCTime -> AddForm -> Bookmark
    toBookmark userId time AddForm {..} =
      Bookmark userId url
        (fromMaybe "" title)
        (maybe "" unTextarea description)
        time
        (maybe True not private)
        (fromMaybe False toread)
        False
    upsertDB :: Bookmark -> [Text] -> DB ()
    upsertDB bookmark tags = do
      let userId = bookmarkUserId bookmark
          url = bookmarkHref bookmark
      getBy (UniqueUserHref userId url) >>= \case
        Just (Entity eid _) -> deleteCascade eid
        _ -> pure ()
      bid <- insert bookmark
      forM_ (zip [1 ..] tags) $
        \(i, tag) -> void $ insert $ BookmarkTag userId tag bid i

-- add widget

viewAddWidget :: Widget -> Maybe Widget -> Text -> Handler Html
viewAddWidget formWidget malert focusEl = popupLayout malert $(widgetFile "add")

-- AddForm

data AddForm = AddForm
  { url :: Text
  , title :: Maybe Text
  , description :: Maybe Textarea
  , tags :: Maybe Text
  , private :: Maybe Bool
  , toread :: Maybe Bool
  } deriving (Show, Eq, Read)

mkAddAForm :: Maybe AddForm -> AForm Handler AddForm
mkAddAForm defs = do
    AddForm
      <$> areq urlField (textAttrs $ named "url" "URL") (url <$> defs)
      <*> aopt textField (textAttrs $ named "title" "title") (title <$> defs)
      <*> aopt textareaField (textAreaAttrs $ named "description" "description") (description <$> defs)
      <*> aopt textField (textAttrs $ named "tags" "tags") (tags <$> defs)
      <*> aopt checkBoxField (named "private" "private") (private <$> defs)
      <*> aopt checkBoxField (named "toread" "read later") (toread <$> defs)
  where
    textAttrs = attr ("size", "70")
    textAreaAttrs = attrs [("cols", "70"), ("rows", "4")]
