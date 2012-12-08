module Handler.Blog
    ( getBlogR
    , getArticleR
    , getAuthorR
    )
where

import Import
import qualified Data.Text as T
import Yesod.Auth
import Data.Time
import System.Locale (defaultTimeLocale)

getBlogR :: Handler RepHtml
getBlogR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [] [Desc ArticleAdded]
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "articles")

getArticleR :: ArticleId -> Handler RepHtml
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")

getAuthorR :: T.Text -> Handler RepHtml
getAuthorR author = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [ArticleAuthor ==. author] [Desc ArticleAdded]
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "articles")
