{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

-- Forms

type Form f = Html -> MForm Handler (FormResult f, Widget)

aFormToMaybeGetSuccess
  :: MonadHandler f
  => AForm f a -> f (Maybe a)
aFormToMaybeGetSuccess =
  fmap maybeSuccess . fmap fst . runFormGet . const . fmap fst . aFormToForm

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
