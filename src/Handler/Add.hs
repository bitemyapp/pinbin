
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
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Add where

import Import

handleAddR :: Handler Html
handleAddR = do
  ((addFormPostRes, addFormW), _) <-
    runFormPost . renderTable . mkAddForm =<<
    aFormToMaybeGetSuccess (mkAddForm Nothing)
  case addFormPostRes of
    FormSuccess (AddForm {..}) -> do
      time <- liftIO getCurrentTime
      userId <- requireAuthId
      void $ runDB $ do
        bid <- insert $
          Bookmark userId url
            (fromMaybe "" title)
            (maybe "" unTextarea description)
            time
            (maybe True not private)
            (fromMaybe False toread)
            False
        forM_ (zip [1..] (maybe [] words tags)) $ \(i, tag) ->
          void $ insert $
            BookmarkTag userId tag bid i
      lookupGetParam "next" >>= \case
        Just next -> redirect next
        Nothing -> popupLayout [whamlet| Add Successful <script> window.close() </script> |]
    _ -> popupLayout $(widgetFile "add")

-- AddForm

data AddForm = AddForm
  { url :: Text
  , title :: Maybe Text
  , description :: Maybe Textarea
  , tags :: Maybe Text
  , private :: Maybe Bool
  , toread :: Maybe Bool
  } deriving (Show, Eq, Read)

mkAddForm :: Maybe AddForm -> AForm Handler AddForm
mkAddForm defs = do
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

