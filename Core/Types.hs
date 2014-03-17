{-# LANGUAGE TemplateHaskell #-}
module Core.Types where

import Prelude
import Database.Persist.TH


-- | The different types of content a piece of content can be.
data ContentKind = Article | Page
    deriving (Show, Read, Ord, Eq)

-- | Derive the persistent field for the `ContentKind` type.
derivePersistField "ContentKind"
