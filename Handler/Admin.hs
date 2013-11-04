{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin where

import Import
import Yesod.Auth


getAdminR :: Handler Html
getAdminR = do
    userEmail <- fmap usersEmail maybeAuth
    adminLayout $ do
        setTitle "Admin: Dashboard"
        $(widgetFile "admin/dashboard")
