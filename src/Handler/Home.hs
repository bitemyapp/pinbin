
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do $(widgetFile "homepage")
