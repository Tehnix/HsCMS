{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Blog where

import Import
import Yesod.Auth
import Yesod.Default.Config (appExtra)
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Database.Esqueleto as E


-- Fetch a specific article
getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    master <- getYesod
    article <- runDB $ get404 articleId
    if not (articleVisible article)
        then notFound
        else defaultLayout $ do
            setTitle $ toHtml $ articleTitle article
            $(widgetFile "theme/default/blog/single-article")

-- Fetch all articles with their author info
pullArticles :: Handler [(Entity Article, Entity User)]
pullArticles = runDB $ E.select $
    E.from $ \(a, u) -> do
    E.where_ (a E.^. ArticleAuthor E.==. u E.^. UserId E.&&. a E.^. ArticleVisible E.==. E.val True E.&&. a E.^. ArticleTrash E.==. E.val False)
    E.orderBy [E.desc (a E.^. ArticleAdded)]
    return (a, u)

getArticlesR :: Handler Html
getArticlesR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    articles <- pullArticles
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "theme/default/blog/articles")

getArchivesR :: Handler Html
getArchivesR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    articles <- pullArticles
    defaultLayout $ do
        setTitle "Archives"
        $(widgetFile "theme/default/blog/archives")

getAuthorR :: UserId -> Handler Html
getAuthorR author = do
    maid <- maybeAuthId
    muser <- maybeAuth
    articles <- pullArticles
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "theme/default/blog/articles")

