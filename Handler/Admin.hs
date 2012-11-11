{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin where

import Import
import Yesod.Auth

isAdmin :: User -> Bool
isAdmin user = userIdent user == "christianlaustsen@gmail.com"

getAdminR :: Handler RepHtml
getAdminR = do
    maid <- maybeAuthId
    defaultLayout $ do
        setTitle "Admin Area"
        $(widgetFile "admin")
    

