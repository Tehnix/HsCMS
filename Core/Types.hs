{-# LANGUAGE TemplateHaskell #-}
module Core.Types where

import Prelude
import Database.Persist.TH


data ContentKind = Article | Page
    deriving (Show, Read, Ord, Eq)
derivePersistField "ContentKind"
