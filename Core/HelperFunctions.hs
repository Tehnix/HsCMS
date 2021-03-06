{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Core.HelperFunctions where

import           Prelude              as Import hiding (head, init, last, readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))
import           Data.Text            as Import (Text, pack, unpack, toLower)
import           Core.Model           as Import
-- #if __GLASGOW_HASKELL__ >= 704
-- import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat), (<>))
-- #else
-- import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat))
-- infixr 5 <>
-- (<>) :: Monoid m => m -> m -> m
-- (<>) = mappend
-- #endif

import           Data.Digest.Pure.MD5
import           Data.ByteString.Lazy.UTF8 as L
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Char as C


-- | Converts spaces to dashes (neat for prettier URLs).
spacesToDashes :: Text -> Text
spacesToDashes t = T.toLower $ T.concatMap (\y -> if y == ' ' then T.pack "-" else T.pack [y]) t

-- | Extract a key from persistent.
extractKey :: KeyBackend backend entity -> String
extractKey = extractKey' . unKey
  where extractKey' (PersistInt64 k) = show k
        extractKey' _ = ""

-- | Add thousand separators.
addSeparator' :: String -> String -> Int -> String
addSeparator' s t 0 = addSeparator' (P.tail s) (P.head s : t) 1
addSeparator' s t n
    | null s = t
    | rem n 3 == 0 = addSeparator' (P.tail s) (P.head s : ',' : t) (n+1)
    | otherwise = addSeparator' (P.tail s) (P.head s : t) (n+1)
-- | Wrapper for the addSeparator function, which adds thousand separators.
addSeparator :: String -> Text
addSeparator s = pack (addSeparator' (P.reverse s) "" 0)

-- | Split the word at @ (at).
splitByAt :: Text -> Text
splitByAt t = pack $ takeWhile (/='@') (unpack t)

-- | Convert the user email to lowercase and md5 hash it
lowerEmailHash :: Maybe (Entity User) -> Text
lowerEmailHash (Just (Entity _ user)) = pack $ show $ md5 . L.fromString $ unpack $ toLower $ userIdent user
lowerEmailHash Nothing = ""

-- | Get the users email.
usersEmail :: Maybe (Entity User) -> Text
usersEmail (Just (Entity _ user)) = userIdent user
usersEmail Nothing = "Unknown"

-- | Convert something to a string and lowercase it.
showToLower :: (Show a) => a -> String
showToLower s = map C.toLower $ show s
