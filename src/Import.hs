{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

-- Forms

type MonadHandlerForm m = (RenderMessage App FormMessage, HandlerSite m ~ App, MonadHandler m)

type Form f = Html -> MForm Handler (FormResult f, Widget)


runPost
  :: (MkAForm a, FromJSON a, MonadHandlerForm m)
  => m (Maybe a)
runPost = aFormToMaybePostSuccess mkAForm

class MkAForm a where
  mkAForm :: MonadHandlerForm m => AForm m a

aFormToMaybeGetSuccess
  :: MonadHandler f
  => AForm f a -> f (Maybe a)
aFormToMaybeGetSuccess =
  fmap maybeSuccess . fmap fst . runFormGet . const . fmap fst . aFormToForm

aFormToMaybePostSuccess
  :: MonadHandlerForm f
  => AForm f a -> f (Maybe a)
aFormToMaybePostSuccess =
  fmap maybeSuccess . fmap fst . runFormPostNoToken . const . fmap fst . aFormToForm

maybeSuccess :: FormResult a -> Maybe a
maybeSuccess (FormSuccess a) = Just a
maybeSuccess _ = Nothing


-- FieldSettings

named :: Text -> FieldSettings master -> FieldSettings master
named n f =
  f
  { fsName = Just n
  , fsId = Just n
  }

attr :: (Text,Text) -> FieldSettings master -> FieldSettings master
attr n f =
  f
  { fsAttrs = n : fsAttrs f
  }

attrs :: [(Text,Text)] -> FieldSettings master -> FieldSettings master
attrs n f =
  f
  { fsAttrs = n ++ fsAttrs f
  }

cls :: [Text] -> FieldSettings master -> FieldSettings master
cls n = attrs [("class", intercalate " " n)]
