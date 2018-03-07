
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Add where

import Import

handleAddR :: Handler Html
handleAddR = do
  ((postRes, addFormW), _) <-
    runFormPost . renderTable . addForm =<<
    aFormToMaybeGetSuccess (addForm Nothing)
  case postRes of
    FormSuccess _ -> do
      lookupGetParam "next" >>= \case
        Just next -> redirect next
        Nothing -> popupLayout [whamlet| Add Successful <script> window.close() |]
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

addForm :: Maybe AddForm -> AForm Handler AddForm
addForm defs = do
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

