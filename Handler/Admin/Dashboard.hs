{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Dashboard where

import Import
import Yesod.Auth


getAdminDashboardR :: Handler Html
getAdminDashboardR = do
    userEmail <- fmap usersEmail maybeAuth
    adminLayout $ do
        setTitle "Admin: Dashboard"
        $(widgetFile "admin/dashboard")
