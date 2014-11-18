{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Content where

import Import


getAdminContentR :: Handler Html
getAdminContentR = adminLayout $ do
        setTitle "Admin: Content"
        $(widgetFile "admin/content")

