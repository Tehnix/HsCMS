module Handler.Blog
    ( getBlogR
    , getArticleR
    )
where

import Import
import Yesod.Auth


getBlogR :: Handler RepHtml
getBlogR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    defaultLayout $ do
        $(widgetFile "articles")

getArticleR :: ArticleId -> Handler RepHtml
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")
