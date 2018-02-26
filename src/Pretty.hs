{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty where
  
import Import

import Text.Show.Pretty (ppShow)
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

cpprint :: (MonadIO m, Show a) => a -> m ()
cpprint = putStrLn . pack . hscolour TTY defaultColourPrefs False False "" False . ppShow

cprint :: (MonadIO m, Show a) => a -> m ()
cprint = putStrLn . pack . hscolour TTY defaultColourPrefs False False "" False . show

pprint :: (MonadIO m, Show a) => a -> m ()
pprint = putStrLn . pack .ppShow
