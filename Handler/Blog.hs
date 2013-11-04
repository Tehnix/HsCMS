{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Blog where

import Import
import Yesod.Auth
import Data.Time
import System.Locale (defaultTimeLocale)


getBlogR :: Handler Html
getBlogR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [] [Desc ArticleAdded]
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "blog/articles")

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "blog/article")

getArchivesR :: Handler Html
getArchivesR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [] [Desc ArticleAdded]
    defaultLayout $ do
        setTitle "Archives"
        $(widgetFile "blog/archives")

getAuthorR :: Text -> Handler Html
getAuthorR author = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [ArticleAuthor ==. author] [Desc ArticleAdded]
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "blog/articles")

