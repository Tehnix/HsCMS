{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.About where

import Import


getAboutR :: Handler Html
getAboutR = do
    defaultLayout $ do
        setTitle "About"
        $(widgetFile $ "theme/default/pages/about")
