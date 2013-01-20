module Handler.About ( getAboutR ) where

import Import

getAboutR :: Handler RepHtml
getAboutR = do
    defaultLayout $ do
        setTitle "About"
        $(widgetFile "about")