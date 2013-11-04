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


-- Add thousand separators
addSeparator' :: String -> String -> Int -> String
addSeparator' s t 0 = addSeparator' (P.tail s) ((P.head s) : t) (1)
addSeparator' s t n = if (P.length s) == 0
    then t
    else if (rem n 3) == 0
        then addSeparator' (P.tail s) ((P.head s) : ',' : t) (n+1)
        else addSeparator' (P.tail s) ((P.head s) : t) (n+1)
-- Wrapper for the addSeparator function, which adds thousand separators
addSeparator :: String -> Text
addSeparator s = pack (addSeparator' (P.reverse s) "" 0)

splitByAt :: Text -> Text
splitByAt t = pack $ takeWhile (/='@') (unpack t)

-- Convert the user email to lowercase and md5 hash it
lowerEmailHash :: Maybe (Entity (UserGeneric backend)) -> String
lowerEmailHash (Just (Entity _ user)) = show $ md5 . L.fromString $ unpack $ toLower $ userIdent user
lowerEmailHash Nothing = ""

-- Get the users email
usersEmail :: Maybe (Entity (UserGeneric backend)) -> Text
usersEmail (Just (Entity _ user)) = userIdent user
usersEmail Nothing = "Unknown"

adminLayout :: Widget -> Handler Html
adminLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    (title', parents) <- breadcrumbs
    userEmail <- fmap usersEmail maybeAuth
    emailHash <- fmap lowerEmailHash maybeAuth
    
    pc <- widgetToPageContent $ do
        $(widgetFile "normalize")
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheetRemote "/static/css/fonts.css"
        addScriptRemote "/static/js/jquery.js"
        addScriptRemote "/static/js/bootstrap.min.js"
        addScriptRemote "/static/js/textAreaExpander.js"
        addScriptRemote "/static/js/showdown.js"
        addScriptRemote "/static/js/extensions/github.js"
        $(widgetFile "admin/admin-layout")
    hamletToRepHtml $(hamletFile "templates/admin/admin-layout-wrapper.hamlet")
 