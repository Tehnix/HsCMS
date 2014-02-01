{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Blog (
      getAdminShowArticlesR
    , getAdminNewArticleR
    , postAdminNewArticleR
    , getAdminUpdateArticleR
    , postAdminUpdateArticleR
    , getAdminShowTrashArticlesR
    , postAdminTrashArticleR
    , postAdminUnpublishArticleR
    , postAdminPublishArticleR
  ) where

import           Import
import           Yesod.Auth
import           System.Locale (defaultTimeLocale)
import           Data.Time
import           Data.Text.Lazy (toStrict)
import           Data.HashMap.Strict (fromList)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Language as EI
import           API.Gist

-- Fetch all articles with their author info
pullArticles :: (EI.From query expr backend (expr (Entity Article)), EI.From query expr backend (expr (Entity User))) => Bool -> query (expr (Entity Article), expr (Entity User))
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
        setTitleI MsgTitleAdminBlogArticles
        toWidget [lucius| #navigation .navigation-articles { background: red; } |]
        $(widgetFile "admin/articles")

-- The form page for posting a new blog post
getAdminNewArticleR :: Handler Html
getAdminNewArticleR = do
    formroute <- return $ AdminNewArticleR
    marticle <- return $ Nothing
    adminLayout $ do
        addScript $ StaticR js_showdown_js
        addScript $ StaticR js_extensions_github_js
        setTitleI MsgTitleAdminNewArticle
        toWidget [lucius| #navigation .navigation-new-article { background: red; } |]
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
    
    -- Either save a draft of the post, or publish it
    case publish of
        Nothing -> do
            articleId <- runDB $ insert $ Article title mdContent htmlContent wordCount added userId Nothing False False
            setMessageI $ MsgMsgSavedArticle $ title
            redirect (AdminUpdateArticleR articleId)
        Just _ -> do
            setMessageI $ MsgMsgCreatedArticle $ title
            
            -- Create a gist of the post if the GitHub PA Token is set
            extra <- getExtra
            gistIdent <- case (extraGithubToken extra) of
                Nothing -> return Nothing
                Just gToken -> do
                    let mdCont = toStrict (renderHtml mdContent)
                    res <- createGist (Just (GitHubToken gToken)) $ Gist title (maybe True (\r -> r) (extraGistPublic extra)) $ fromList [(title <> ".md", (GistContent mdCont Nothing))]
                    case res of
                        Nothing -> do
                            setMessageI $ MsgMsgCreatedArticleGistError $ title
                            return Nothing
                        Just (GistResponse gId) -> return $ Just gId
            
            _ <- runDB $ insert $ Article title mdContent htmlContent wordCount added userId gistIdent True False
            redirect AdminShowArticlesR

-- The form page for updating an existing blog post
getAdminUpdateArticleR :: ArticleId -> Handler Html
getAdminUpdateArticleR articleId = do
    formroute <- return $ AdminUpdateArticleR articleId
    dbarticle <- runDB $ get404 articleId
    marticle <- return $ Just dbarticle
    adminLayout $ do
        addScript $ StaticR js_showdown_js
        addScript $ StaticR js_extensions_github_js
        case marticle of
            Just _ -> setTitleI MsgTitleAdminUpdateArticle
            Nothing -> setTitleI MsgTitleAdminNewArticle
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
    
    -- Update the gist of the post if the GitHub PA Token is set
    originalArticle <- runDB $ get404 articleId
    extra <- getExtra
    gistIdent <- case (extraGithubToken extra) of
        Nothing -> return Nothing
        Just gToken -> do
            let mdCont = toStrict (renderHtml mdContent)
            res <- case (articleGistId originalArticle) of
                Nothing -> do
                    -- If the article doesn't have a gist ID already
                    return =<< createGist (Just (GitHubToken gToken)) $ Gist title (maybe True (\r -> r) (extraGistPublic extra)) $ fromList [(title <> ".md", (GistContent mdCont Nothing))]
                Just gId -> do
                    return =<< updateGist (GitHubToken gToken) gId $ Gist title True $ fromList [((articleTitle originalArticle) <> ".md", (GistContent mdCont (Just ((articleTitle originalArticle) <> ".md"))))]
            case res of
                Nothing -> return Nothing
                Just (GistResponse gId) -> return $ Just gId
    
    -- Set the Bool and message depending on whether the post is published or unpublished
    publishStatus <- case unpublish of
        Nothing -> do
            setMessageI $ MsgMsgPublishedArticle $ title
            return True
        Just _ -> do
            setMessageI $ MsgMsgUnpublishedArticle $ title
            return False
    
    -- Either save the post (ignoring if it's published or not), or change the publish status of the post
    case publish of
        Nothing -> do
            runDB $ update articleId [ArticleGistId =. gistIdent, ArticleVisible =. publishStatus, ArticleTitle =. title, ArticleMdContent =. mdContent, ArticleHtmlContent =. htmlContent, ArticleWordCount =. wordCount]
            if publishStatus then
                setMessageI $ MsgMsgSavedArticle $ title 
                else setMessageI $ MsgMsgUnpublishedArticle $ title
            redirect (AdminUpdateArticleR articleId)
        Just _ -> do
            runDB $ update articleId [ArticleGistId =. gistIdent, ArticleVisible =. publishStatus, ArticleTitle =. title, ArticleMdContent =. mdContent, ArticleHtmlContent =. htmlContent, ArticleWordCount =. wordCount]
            redirect AdminShowArticlesR

getAdminShowTrashArticlesR :: Handler Html
getAdminShowTrashArticlesR = do
    let isTrashRoute = True
    articles <- runDB $ E.select $ pullArticles True
    -- articles <- runDB $ selectList [ArticleTrash ==. True] [Desc ArticleAdded]
    adminLayout $ do
        setTitleI MsgTitleAdminTrashArticles
        toWidget [lucius| #navigation .navigation-trash { background: red; } |]
        $(widgetFile "admin/articles")

-- Deleting a blog post
postAdminTrashArticleR :: ArticleId -> Handler Html
postAdminTrashArticleR articleId = do
    runDB $ update articleId [ArticleTrash =. True]
    article <- runDB $ get404 articleId
    setMessageI $ MsgMsgDeletedArticle $ (articleTitle article)
    redirect AdminShowArticlesR

-- Unpublish the blog post
postAdminUnpublishArticleR :: ArticleId -> Handler Html
postAdminUnpublishArticleR articleId = do
    runDB $ update articleId [ArticleVisible =. False]
    article <- runDB $ get404 articleId
    setMessageI $ MsgMsgUnpublishedArticle $ (articleTitle article)
    redirect AdminShowArticlesR

-- Publish the blog post
postAdminPublishArticleR :: ArticleId -> Handler Html
postAdminPublishArticleR articleId = do
    runDB $ update articleId [ArticleVisible =. True]
    article <- runDB $ get404 articleId
    setMessageI $ MsgMsgPublishedArticle $ (articleTitle article)
    redirect AdminShowArticlesR

