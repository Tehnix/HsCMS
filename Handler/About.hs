{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.About where

import Import


getAboutR :: Handler Html
getAboutR = defaultLayout $ 
    do
        setTitle "About"
        $(widgetFile "pages/about")

