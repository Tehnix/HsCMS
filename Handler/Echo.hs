module Handler.Echo where

import Import

getEchoR :: Text -> Handler RepHtml
getEchoR theText = do
    defaultLayout $ do
        $(widgetFile "echo")

