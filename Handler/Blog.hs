{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Blog where

import Import
import Yesod.Default.Config (appExtra)
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Database.Esqueleto as E


-- | Fetch all articles with their author information
pullArticles :: Handler [(Entity Article, Entity User)]
pullArticles = runDB $ E.select $
    E.from $ \(a, u) -> do
    E.where_ (a E.^. ArticleAuthor E.==. u E.^. UserId 
        E.&&. a E.^. ArticleVisible E.==. E.val True 
        E.&&. a E.^. ArticleTrash E.==. E.val False)
    E.orderBy [E.desc (a E.^. ArticleAdded)]
    return (a, u)

-- | Article widget
blogArticle :: ArticleId -> Article -> Bool -> Widget
blogArticle articleId article comments = do
    master <- getYesod
    allowComments <- return comments
    $(widgetFile "blog/single-article")

-- | Display a single article
getArticleR :: ArticleId -> Text -> Handler Html
getArticleR articleId _ = do
    master <- getYesod
    article <- runDB $ get404 articleId
    allowComments <- return True
    if not (articleVisible article)
        then notFound
        else defaultLayout $ do
            setTitle $ toHtml $ articleTitle article
            $(widgetFile "blog/single-article")

-- | Display all articles
getArticlesR :: Handler Html
getArticlesR = do
    articles <- pullArticles
    allowComments <- return False
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "blog/articles")

-- | Display a archive of all articles
getArchivesR :: Handler Html
getArchivesR = do
    articles <- pullArticles
    defaultLayout $ do
        setTitle "Archives"
        $(widgetFile "blog/archives")

-- | Display all articles from a specific author
getAuthorR :: UserId -> Handler Html
getAuthorR author = do
    articles <- pullArticles
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "blog/articles")

