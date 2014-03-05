{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.About where

import Core.Import


getAboutR :: Handler Html
getAboutR = defaultLayout $ do
    setTitle "About"
    $(widgetFile "front/about")

