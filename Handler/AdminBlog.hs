module Handler.AdminBlog
    ( getAdminBlogR
    , getAdminBlogNewR
    , postAdminBlogNewR
    , getAdminBlogPostR
    , postAdminBlogPostR
    , postAdminBlogDeleteR
    )
where
    
import Import
import Yesod.Auth
import Data.Time
import System.Locale (defaultTimeLocale)


-- The view showing the list of articles
getAdminBlogR :: Handler RepHtml
getAdminBlogR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [] [Desc ArticleAdded]
    adminLayout $ do
        setTitle "Admin: Blog Posts"
        $(widgetFile "admin-blog")
             
-- The form page for posting a new blog post 
getAdminBlogNewR :: Handler RepHtml
getAdminBlogNewR = do
    formroute <- return $ AdminBlogNewR
    marticle <- return $ Nothing
    adminLayout $ do
        setTitle "Admin: New Post"
        $(widgetFile "admin-blogPost")


-- Handling the new posted blog post
postAdminBlogNewR :: Handler ()
postAdminBlogNewR = do
    title <- runInputPost $ ireq textField "form-title-field"
    mdContent <- runInputPost $ ireq htmlField "form-mdcontent-field"
    htmlContent <- runInputPost $ ireq htmlField "form-htmlcontent-field"
    wordCount <- runInputPost $ ireq intField "form-wordcount-field"
    added <- liftIO getCurrentTime
    author <- fmap usersEmail maybeAuth
    _ <- runDB $ insert $ Article title mdContent htmlContent wordCount added author 1
    setMessage $ "Post Created"
    redirect AdminBlogR

-- The form page for updating an existing blog post
getAdminBlogPostR :: ArticleId -> Handler RepHtml
getAdminBlogPostR articleId = do
    formroute <- return $ AdminBlogPostR articleId
    dbarticle <- runDB $ get404 articleId
    marticle <- return $ Just dbarticle
    adminLayout $ do
        setTitle "Admin: New Post"
        $(widgetFile "admin-blogPost")

-- Handling the updated blog post
postAdminBlogPostR :: ArticleId -> Handler ()
postAdminBlogPostR articleId = do
    title <- runInputPost $ ireq textField "form-title-field"
    mdContent <- runInputPost $ ireq htmlField "form-mdcontent-field"
    htmlContent <- runInputPost $ ireq htmlField "form-htmlcontent-field"
    wordCount <- runInputPost $ ireq intField "form-wordcount-field"
    runDB $ update articleId [ArticleTitle =. title, ArticleMdContent =. mdContent, ArticleHtmlContent =. htmlContent, ArticleWordCount =. wordCount]
    setMessage $ "Post Update"
    redirect AdminBlogR

-- Handling the updated blog post
postAdminBlogDeleteR :: ArticleId -> Handler ()
postAdminBlogDeleteR articleId = do
    runDB $ update articleId [ArticleVisible =. 0]
    setMessage $ "Post Deleted"
    redirect AdminBlogR
