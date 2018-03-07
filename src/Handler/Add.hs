
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.Add where

import Import
-- import Database.Persist.Sql
-- import qualified Database.Esqueleto as E

data AddForm = AddForm
  { next :: Maybe Text
  , url :: Text
  , title :: Text
  , description :: Maybe Text
  , tags :: Maybe Text
  , private :: Maybe Bool
  , toread :: Maybe Bool
  } deriving (Show, Eq, Read)

getAddR :: Handler Html
getAddR = do
  ((res, widget), enctype) <- runFormGet addForm 
  gets <- liftM reqGetParams getRequest
  cpprint gets
  cpprint res
  popupLayout $ do
    widget

postAddR :: Handler Html
postAddR = undefined

addForm :: Html -> MForm Handler (FormResult AddForm, Widget)
addForm extra = do
  (nextR, nextV) <-
    mopt hiddenField ((fromString "") {fsId = Just "next", fsName = Just "next"}) Nothing
  (urlR, urlV) <-
    mreq textField ((fromString "") {fsId = Just "url", fsName = Just "url"}) Nothing
  (titleR, titleV) <-
    mreq textField ((fromString "") {fsId = Just "title", fsName = Just "title"}) Nothing
  (descriptionR, descriptionV) <-
    mopt textareaField ((fromString "") {fsId = Just "description", fsName = Just "description"}) Nothing
  (tagsR, tagsV) <-
    mopt textField ((fromString "") {fsId = Just "tags", fsName = Just "tags"}) Nothing
  (privateR, privateV) <-
    mopt checkBoxField ((fromString "") {fsId = Just "private", fsName = Just "private"}) Nothing
  (toreadR, toreadV) <-
    mopt checkBoxField ((fromString "") {fsId = Just "toread", fsName = Just "toread"}) Nothing
  let addFormR =
        AddForm <$> nextR <*> urlR <*> titleR <*> ((fmap . fmap) unTextarea descriptionR) <*> tagsR <*> privateR <*> toreadR
      addFormW =
        $(widgetFile "add")
  pure (addFormR, addFormW)

