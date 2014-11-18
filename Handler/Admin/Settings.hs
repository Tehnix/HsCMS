{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Settings where

import Import


getAdminSettingsR :: Handler Html
getAdminSettingsR = do
    adminLayout $ do
        setTitle "Admin: Settings"
        $(widgetFile "admin/settings")

