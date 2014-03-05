{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module GeneralFunctions where

import           Prelude              as Import hiding (head, init, last, readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))
-- import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text, pack, unpack, toLower)
import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
-- import           Settings.Development as Import
import           Settings.StaticFiles as Import
-- #if __GLASGOW_HASKELL__ >= 704
-- import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat), (<>))
-- #else
-- import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat))
-- infixr 5 <>
-- (<>) :: Monoid m => m -> m -> m
-- (<>) = mappend
-- #endif

import Yesod.Auth
import Text.Hamlet (hamletFile)
import Yesod.Default.Config (appExtra)
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.UTF8 as L
import qualified Prelude as P
import qualified Data.Text as T


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
addSeparator' s t 0 = addSeparator' (P.tail s) ((P.head s) : t) 1
addSeparator' s t n = if (P.length s) == 0
    then t
    else if (rem n 3) == 0
        then addSeparator' (P.tail s) ((P.head s) : ',' : t) (n+1)
        else addSeparator' (P.tail s) ((P.head s) : t) (n+1)
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

-- | The layout for the admin panel.
adminLayout :: Widget -> Handler Html
adminLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    (title', parents) <- breadcrumbs
    userEmail <- fmap usersEmail maybeAuth
    emailHash <- fmap lowerEmailHash maybeAuth
    pc <- widgetToPageContent $ do
        $(combineStylesheets 'StaticR
            [ css_normalize_css
            , css_bootstrap_css
            , css_fonts_css
            ])
        $(combineScripts 'StaticR
            [ js_jquery_js
            , js_bootstrap_min_js
            ])
        $(widgetFile "admin/layout")
    giveUrlRenderer $(hamletFile "templates/admin/layout-wrapper.hamlet")

