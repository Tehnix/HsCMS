{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Articles where

import Core.Import
import Yesod.Default.Config (appExtra)
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Database.Esqueleto as E


-- | Fetch all articles with their author information
pullArticles :: Handler [(Entity Content, Entity User)]
pullArticles = runDB $ E.select $
    E.from $ \(c, u) -> do
    E.where_ (c E.^. ContentAuthor E.==. u E.^. UserId
        E.&&. c E.^. ContentType E.==. E.val Article
        E.&&. c E.^. ContentVisible E.==. E.val True
        E.&&. c E.^. ContentTrash E.==. E.val False)
    E.orderBy [E.desc (c E.^. ContentAdded)]
    return (c, u)

-- | Article widget
blogArticle :: ContentId -> Content -> Bool -> Widget
blogArticle contentId content comments = do
    master <- getYesod
    let allowComments = comments
    $(widgetFile "front/single-article")

-- | Display a single article
getArticleR :: ContentId -> Text -> Handler Html
getArticleR contentId _ = do
    master <- getYesod
    content <- runDB $ get404 contentId
    let allowComments = True
    if not (contentVisible content)
        then notFound
        else defaultLayout $ do
            setTitle $ toHtml $ contentTitle content
            $(widgetFile "front/single-article")

-- | Display all articles
getArticlesR :: Handler Html
getArticlesR = do
    allContent <- pullArticles
    let allowComments = False
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "front/articles")

-- | Display a archive of all articles
getArchivesR :: Handler Html
getArchivesR = do
    allContent <- pullArticles
    defaultLayout $ do
        setTitle "Archives"
        $(widgetFile "front/archives")

-- | Display all articles from a specific author
getAuthorR :: UserId -> Handler Html
getAuthorR author = do
    allContent <- pullArticles
    defaultLayout $ do
        setTitle "Articles"
        $(widgetFile "front/articles")
