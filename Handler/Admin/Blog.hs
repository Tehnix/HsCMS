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

import           Core.Import
import           Yesod.Auth
import           System.Locale (defaultTimeLocale)
import           Data.Time
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Language as EI
import           Handler.Admin.CreateContent


{-|
  This block generalises the templates so they can be used with other
  content.
-}
contentTitle :: Article -> Text
contentTitle = articleTitle
contentMarkdown :: Article -> Html
contentMarkdown = articleMdContent
contentHtmlContent :: Article -> Html
contentHtmlContent = articleHtmlContent
contentVisible :: Article -> Bool
contentVisible = articleVisible
contentAdded :: Article -> UTCTime
contentAdded = articleAdded
contentWordCount :: Article -> Int
contentWordCount = articleWordCount
updateRoute :: ArticleId -> Route App
updateRoute = AdminUpdateArticleR
viewRoute :: ArticleId -> Text -> Route App
viewRoute = ArticleR
unpublishRoute :: ArticleId -> Route App
unpublishRoute = AdminUnpublishArticleR
publishRoute :: ArticleId -> Route App
publishRoute = AdminPublishArticleR
trashRoute :: ArticleId -> Route App
trashRoute = AdminTrashArticleR
msgContentSingle :: AppMessage
msgContentSingle = MsgArticle
msgContentPlural :: AppMessage
msgContentPlural = MsgArticles
msgNoContent :: AppMessage
msgNoContent = MsgNoArticles

-- | Fetch all articles with their author information
pullArticles :: (EI.From query expr backend (expr (Entity Article)),
                 EI.From query expr backend (expr (Entity User))) =>
                 Bool -> query (expr (Entity Article), expr (Entity User))
pullArticles trash = E.from $ \(a, u) -> do
    E.where_ (a E.^. ArticleAuthor E.==. u E.^. UserId
        E.&&. a E.^. ArticleTrash E.==. E.val trash)
    E.orderBy [E.desc (a E.^. ArticleAdded)]
    return (a, u)

-- | View all articles
getAdminShowArticlesR :: Handler Html
getAdminShowArticlesR = do
    msgRender <- getMessageRender
    let isTrashRoute = False
    contentList <- runDB $ E.select $ pullArticles False
    adminLayout $ do
        setTitleI MsgTitleAdminBlogArticles
        toWidget [lucius|
            #navigation .navigation-articles { background: red; }
            #navigation .navigation-new-article { display: block !important; }
            #navigation .navigation-trash-articles { display: block !important; }
        |]
        $(widgetFile "admin/list-content")

-- | Form page for creating a new article
getAdminNewArticleR :: Handler Html
getAdminNewArticleR = do
    let formroute = AdminNewArticleR
    let mcontent = Nothing
    adminLayout $ do
        addScript $ StaticR js_showdown_js
        addScript $ StaticR js_extensions_github_js
        setTitleI MsgTitleAdminNewArticle
        toWidget [lucius|
            #navigation .navigation-new-article { background: red !important; display: block !important; }
            #navigation .navigation-trash-articles { display: block !important; }
        |]
        $(widgetFile "admin/create-content")

-- | Handling the form for creating an article
postAdminNewArticleR :: Handler Html
postAdminNewArticleR = do
    title <- runInputPost $ ireq textField "form-title-field"
    mdContent <- runInputPost $ ireq htmlField "form-mdcontent-field"
    htmlContent <- runInputPost $ ireq htmlField "form-htmlcontent-field"
    wordCount <- runInputPost $ ireq intField "form-wordcount-field"
    saved <- runInputPost $ iopt boolField "form-saved"
    added <- liftIO getCurrentTime
    userId <- requireAuthId
    -- Either save a draft of the post, or publish it
    case saved of
        Nothing -> do
            gistIdent <- maybeCreateOrUpdateGist Nothing "" title mdContent MsgMsgCreatedArticleGistError
            _ <- runDB $ insert $ Article title mdContent htmlContent wordCount added userId gistIdent True False
            setMessageI $ MsgMsgCreatedArticle title
            redirect AdminShowArticlesR
        Just _ -> do
            articleId <- runDB $ insert $ Article title mdContent htmlContent wordCount added userId Nothing False False
            setMessageI $ MsgMsgSavedArticle title
            redirect (AdminUpdateArticleR articleId)

-- | Form page for updating an article
getAdminUpdateArticleR :: ArticleId -> Handler Html
getAdminUpdateArticleR articleId = do
    let formroute = AdminUpdateArticleR articleId
    dbcontent <- runDB $ get404 articleId
    let mcontent = Just dbcontent
    adminLayout $ do
        addScript $ StaticR js_showdown_js
        addScript $ StaticR js_extensions_github_js
        setTitleI MsgTitleAdminUpdateArticle
        toWidget [lucius|
            #navigation .navigation-new-article { display: block !important; }
            #navigation .navigation-trash-articles { display: block !important; }
        |]
        $(widgetFile "admin/create-content")

-- | Handling the form for updating an article
postAdminUpdateArticleR :: ArticleId -> Handler Html
postAdminUpdateArticleR articleId = do
    title <- runInputPost $ ireq textField "form-title-field"
    mdContent <- runInputPost $ ireq htmlField "form-mdcontent-field"
    htmlContent <- runInputPost $ ireq htmlField "form-htmlcontent-field"
    wordCount <- runInputPost $ ireq intField "form-wordcount-field"
    saved <- runInputPost $ iopt boolField "form-saved"
    unpublish <- runInputPost $ iopt boolField "form-unpublish"
    updated <- liftIO getCurrentTime

    original <- runDB $ get404 articleId
    gistIdent <- maybeCreateOrUpdateGist (articleGistId original) (articleTitle original) title mdContent MsgMsgCreatedArticleGistError
    -- Handle changing the visible status and redirecting to the appropriate page
    case unpublish of
        Nothing -> do
            setMessageI $ MsgMsgPublishedArticle title
            wasSaved True saved AdminShowArticlesR title gistIdent mdContent htmlContent wordCount updated
        Just _ -> do
            setMessageI $ MsgMsgUnpublishedArticle title
            wasSaved False saved (AdminUpdateArticleR articleId) title gistIdent mdContent htmlContent wordCount (articleAdded original)
    where
        wasSaved publish saved redirectRoute t g mC hC wC updated =
            case saved of
                Nothing -> do
                    runDB $ update articleId [ ArticleGistId =. g
                                             , ArticleVisible =. publish
                                             , ArticleTitle =. t
                                             , ArticleMdContent =. mC
                                             , ArticleHtmlContent =. hC
                                             , ArticleWordCount =. wC
                                             , ArticleAdded =. updated ]
                    redirect redirectRoute
                Just _ -> do
                    runDB $ update articleId [ ArticleGistId =. g
                                             , ArticleTitle =. t
                                             , ArticleMdContent =. mC
                                             , ArticleHtmlContent =. hC
                                             , ArticleWordCount =. wC ]
                    setMessageI $ MsgMsgSavedArticle t
                    redirect (AdminUpdateArticleR articleId)

-- | View all trashed articles
getAdminShowTrashArticlesR :: Handler Html
getAdminShowTrashArticlesR = do
    msgRender <- getMessageRender
    let isTrashRoute = True
    contentList <- runDB $ E.select $ pullArticles True
    adminLayout $ do
        setTitleI MsgTitleAdminTrashArticles
        toWidget [lucius|
            #navigation .navigation-new-article { display: block !important; }
            #navigation .navigation-trash-articles { background: red !important; display: block !important; }
        |]
        $(widgetFile "admin/list-content")

-- | Mark an article as trashed
postAdminTrashArticleR :: ArticleId -> Handler Html
postAdminTrashArticleR articleId = do
    runDB $ update articleId [ArticleTrash =. True]
    article <- runDB $ get404 articleId
    setMessageI $ MsgMsgDeletedArticle $ articleTitle article
    redirect AdminShowArticlesR

-- | Change the status of an article to unpublished
postAdminUnpublishArticleR :: ArticleId -> Handler Html
postAdminUnpublishArticleR articleId = do
    runDB $ update articleId [ArticleVisible =. False]
    article <- runDB $ get404 articleId
    setMessageI $ MsgMsgUnpublishedArticle $ articleTitle article
    redirect AdminShowArticlesR

-- | Change the status of an article to published
postAdminPublishArticleR :: ArticleId -> Handler Html
postAdminPublishArticleR articleId = do
    updated <- liftIO getCurrentTime
    runDB $ update articleId [ArticleVisible =. True, ArticleAdded =. updated]
    article <- runDB $ get404 articleId
    setMessageI $ MsgMsgPublishedArticle $ articleTitle article
    redirect AdminShowArticlesR
