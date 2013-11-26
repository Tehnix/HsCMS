{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Blog where
    
import Import
import Yesod.Auth
import Data.Time
{-import Data.Maybe (fromJust)-}
import System.Locale (defaultTimeLocale)


-- The view showing the list of articles
getAdminShowArticlesR :: Handler Html
getAdminShowArticlesR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- #{userIdent userData} <-- using userData in hamlet
    {-userId <- requireAuthId-}
    {-Entity _ userData <- runDB $ selectFirst [UserId ==. userId] [] >>= return.fromJust-}
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [] [Desc ArticleAdded]
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
    added <- liftIO getCurrentTime
    author <- fmap usersEmail maybeAuth
    _ <- runDB $ insert $ Article title mdContent htmlContent wordCount added author 1
    setMessage $ "Post Created"
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
    runDB $ update articleId [ArticleTitle =. title, ArticleMdContent =. mdContent, ArticleHtmlContent =. htmlContent, ArticleWordCount =. wordCount]
    setMessage $ "Post Updated"
    redirect AdminShowArticlesR

-- Handling the updated blog post
postAdminDeleteArticleR :: ArticleId -> Handler Html
postAdminDeleteArticleR articleId = do
    runDB $ update articleId [ArticleVisible =. 0]
    setMessage $ "Post Deleted"
    redirect AdminShowArticlesR

