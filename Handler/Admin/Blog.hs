{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Blog where

import Import
import Yesod.Auth
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Database.Esqueleto as E


-- import Data.Maybe (fromJust)
-- #{userIdent user} <-- using userData in hamlet
-- #{articleAuthor article} <-- the original one
-- userId <- requireAuthId
-- Entity _ user <- runDB $ selectFirst [UserId ==. userId] [] >>= return.fromJust

-- Fetch all articles with their author info
pullArticles trash = E.from $ \(a, u) -> do
    E.where_ (a E.^. ArticleAuthor E.==. u E.^. UserId E.&&. a E.^. ArticleTrash E.==. E.val trash)
    E.orderBy [E.desc (a E.^. ArticleAdded)]
    return (a, u)

-- The view showing the list of articles
getAdminShowArticlesR :: Handler Html
getAdminShowArticlesR = do
    let isTrashRoute = False
    articles <- runDB $ E.select $ pullArticles False
    -- articles <- runDB $ selectList [ArticleTrash ==. False] [Desc ArticleAdded]
    adminLayout $ do
        setTitle "Admin: Blog Posts"
        $(widgetFile "admin/articles")

-- The form page for posting a new blog post
getAdminNewArticleR :: Handler Html
getAdminNewArticleR = do
    formroute <- return $ AdminNewArticleR
    marticle <- return $ Nothing
    adminLayout $ do
        setTitle "Admin: New Post"
        $(widgetFile "admin/create-article")

-- Handling the new posted blog post
postAdminNewArticleR :: Handler Html
postAdminNewArticleR = do
    title <- runInputPost $ ireq textField "form-title-field"
    mdContent <- runInputPost $ ireq htmlField "form-mdcontent-field"
    htmlContent <- runInputPost $ ireq htmlField "form-htmlcontent-field"
    wordCount <- runInputPost $ ireq intField "form-wordcount-field"
    publish <- runInputPost $ iopt boolField "form-publish"
    added <- liftIO getCurrentTime
    userId <- requireAuthId
    case publish of
        Nothing -> do
            articleId <- runDB $ insert $ Article title mdContent htmlContent wordCount added userId False False
            setMessage $ "Saved Post: " <> (toHtml title)
            redirect (AdminUpdateArticleR articleId)
        Just _ -> do
            _ <- runDB $ insert $ Article title mdContent htmlContent wordCount added userId True False
            setMessage $ "Created Post: " <> (toHtml title)
            redirect AdminShowArticlesR

-- The form page for updating an existing blog post
getAdminUpdateArticleR :: ArticleId -> Handler Html
getAdminUpdateArticleR articleId = do
    formroute <- return $ AdminUpdateArticleR articleId
    dbarticle <- runDB $ get404 articleId
    marticle <- return $ Just dbarticle
    adminLayout $ do
        case marticle of
            Just _ -> setTitle "Admin: Update post"
            Nothing -> setTitle "Admin: New Post"
        $(widgetFile "admin/create-article")

-- Handling the updated blog post
postAdminUpdateArticleR :: ArticleId -> Handler Html
postAdminUpdateArticleR articleId = do
    title <- runInputPost $ ireq textField "form-title-field"
    mdContent <- runInputPost $ ireq htmlField "form-mdcontent-field"
    htmlContent <- runInputPost $ ireq htmlField "form-htmlcontent-field"
    wordCount <- runInputPost $ ireq intField "form-wordcount-field"
    publish <- runInputPost $ iopt boolField "form-publish"
    unpublish <- runInputPost $ iopt boolField "form-unpublish"
    publishStatus <- case unpublish of
        Nothing -> return True
        Just _ -> return False
    case publish of
        Nothing -> do
            runDB $ update articleId [ArticleTitle =. title, ArticleMdContent =. mdContent, ArticleHtmlContent =. htmlContent, ArticleWordCount =. wordCount]
            setMessage $ "Saved Post: " <> (toHtml title)
            redirect (AdminUpdateArticleR articleId)
        Just _ -> do
            runDB $ update articleId [ArticleVisible =. publishStatus, ArticleTitle =. title, ArticleMdContent =. mdContent, ArticleHtmlContent =. htmlContent, ArticleWordCount =. wordCount]
            if publishStatus then setMessage $ "Published Post: " <> (toHtml title)
                else setMessage $ "Unpublished Post: " <> (toHtml title)
            redirect AdminShowArticlesR

getAdminShowTrashArticlesR :: Handler Html
getAdminShowTrashArticlesR = do
    let isTrashRoute = True
    articles <- runDB $ E.select $ pullArticles True
    -- articles <- runDB $ selectList [ArticleTrash ==. True] [Desc ArticleAdded]
    adminLayout $ do
        setTitle "Admin: Blog Posts"
        $(widgetFile "admin/articles")

-- Deleting a blog post
postAdminTrashArticleR :: ArticleId -> Handler Html
postAdminTrashArticleR articleId = do
    runDB $ update articleId [ArticleTrash =. True]
    article <- runDB $ get404 articleId
    setMessage $ "Deleted Post: " <> (toHtml (articleTitle article))
    redirect AdminShowArticlesR

-- Unpublish the blog post
postAdminUnpublishArticleR :: ArticleId -> Handler Html
postAdminUnpublishArticleR articleId = do
    runDB $ update articleId [ArticleVisible =. False]
    article <- runDB $ get404 articleId
    setMessage $ "Unpublished Post: " <> (toHtml (articleTitle article))
    redirect AdminShowArticlesR

-- Publish the blog post
postAdminPublishArticleR :: ArticleId -> Handler Html
postAdminPublishArticleR articleId = do
    runDB $ update articleId [ArticleVisible =. True]
    article <- runDB $ get404 articleId
    setMessage $ "Published Post: " <> (toHtml (articleTitle article))
    redirect AdminShowArticlesR

