module Handler.Blog
    ( getBlogR
    , getArticleR
    , getAdminBlogR
    , postAdminBlogR
    )
where

import Import
import Yesod.Auth
import Text.Hamlet (hamletFile)
import Yesod.Default.Config (appExtra)

import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.UTF8 as L
import Data.Char
import qualified Data.Text as T

-- Convert a string to a lowercase string
strToLower :: [Char] -> [Char]
strToLower [] = []
strToLower (x:xs) = toLower x : strToLower xs

-- Convert the user email to lowercase and md5 hash it
lowerEmailHash :: Maybe (Entity (UserGeneric backend)) -> String
lowerEmailHash (Just (Entity _ user)) = show $ md5 . L.fromString $ strToLower $ T.unpack $ userIdent user
lowerEmailHash Nothing = ""

usersEmail :: Maybe (Entity (UserGeneric backend)) -> Text
usersEmail (Just (Entity _ user)) = userIdent user
usersEmail Nothing = "Unknown"

adminLayout :: GWidget App App () -> GHandler App App RepHtml
adminLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    userEmail <- fmap usersEmail maybeAuth
    emailHash <- fmap lowerEmailHash maybeAuth
    
    pc <- widgetToPageContent $ do
        $(widgetFile "normalize")
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheetRemote "/static/css/fonts.css"
        addScriptRemote "/static/js/jquery.js"
        addScriptRemote "/static/js/bootstrap.min.js"
        $(widgetFile "admin-layout")
    hamletToRepHtml $(hamletFile "templates/admin-layout-wrapper.hamlet")
    
getBlogR :: Handler RepHtml
getBlogR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet)
    defaultLayout $ do
        $(widgetFile "articles")

getArticleR :: ArticleId -> Handler RepHtml
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")
           
-- Admin section

entryForm :: Form Article
entryForm = renderDivs $ Article
    <$> areq textField "Title" { fsId = Just "article-title-field", fsName = Just "article-title-field"  } Nothing
    <*> areq htmlField "Content" { fsId = Just "article-content-field", fsName = Just "article-content-field" } Nothing
    
-- The view showing the list of articles
getAdminBlogR :: Handler RepHtml
getAdminBlogR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of articles inside the database
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet)
    (articleWidget, enctype) <- generateFormPost entryForm
    adminLayout $ do
        $(widgetFile "admin-writeArticle")

postAdminBlogR :: Handler RepHtml
postAdminBlogR = do
    ((res, articleWidget), enctype) <- runFormPost entryForm
    case res of
        FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
        _ -> adminLayout $ do
            setTitle "Please correct your entry form"
            $(widgetFile "articleAddError")

