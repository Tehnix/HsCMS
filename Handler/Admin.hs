{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin where

import Import
import Yesod.Auth
import qualified Data.Text as T

getAdminR :: Handler RepHtml
getAdminR = do
    userEmail <- fmap usersEmail maybeAuth
    adminLayout $ do
        setTitle "Admin: Dashboard"
        $(widgetFile "admin/dashboard")

    

