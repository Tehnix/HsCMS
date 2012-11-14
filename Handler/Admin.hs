{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin where

import Import
import Yesod.Auth
import Text.Hamlet (hamletFile)
import Yesod.Default.Config (appExtra)

import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.UTF8 as L
import Data.Char
import qualified Data.Text as T

-- Convert a string to a lowercase string
strToLower :: [Char] -> [Char]
strToLower [] = []
strToLower (x:xs) = toLower x : strToLower xs

-- Convert the user email to lowercase and md5 hash it
lowerEmailHash :: Maybe (Entity (UserGeneric backend)) -> String
lowerEmailHash (Just (Entity _ user)) = show $ md5 . L.fromString $ strToLower $ T.unpack $ userIdent user
lowerEmailHash Nothing = ""

usersEmail :: Maybe (Entity (UserGeneric backend)) -> Text
usersEmail (Just (Entity _ user)) = userIdent user
usersEmail Nothing = "Unknown"

adminLayout :: GWidget App App () -> GHandler App App RepHtml
adminLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    userEmail <- fmap usersEmail maybeAuth
    emailHash <- fmap lowerEmailHash maybeAuth
    
    pc <- widgetToPageContent $ do
        $(widgetFile "normalize")
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheetRemote "/static/css/fonts.css"
        addScriptRemote "/static/js/jquery.js"
        addScriptRemote "/static/js/bootstrap.min.js"
        $(widgetFile "admin-layout")
    hamletToRepHtml $(hamletFile "templates/admin-layout-wrapper.hamlet")

-- The url to construct:
-- //www.gravatar.com/avatar/HASH?s=40

splitByAt :: [Char] -> [Char]
splitByAt t = takeWhile (/='@') t

getAdminR :: Handler RepHtml
getAdminR = do
    userEmail <- fmap usersEmail maybeAuth
    adminLayout $ do
        setTitle "Admin Area"
        $(widgetFile "admin")

    

