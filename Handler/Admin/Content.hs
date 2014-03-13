{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Content where

import           Core.Import
import           Yesod.Auth
import           System.Locale (defaultTimeLocale)
import           Data.Time
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Language as EI
import           Handler.Admin.Gist


-- | This block generalises the templates so they can be used with other content.
updateRoute :: ContentKind -> ContentId -> Route App
updateRoute Article = AdminUpdateArticleR
updateRoute Page = AdminUpdateStaticPageR
viewRoute :: ContentKind -> ContentId -> Text -> Route App
viewRoute Article = ArticleR
viewRoute Page = StaticPageR
unpublishRoute :: ContentKind -> ContentId -> Route App
unpublishRoute Article = AdminUnpublishArticleR
unpublishRoute Page = AdminUnpublishStaticPageR
publishRoute :: ContentKind -> ContentId -> Route App
publishRoute Article = AdminPublishArticleR
publishRoute Page = AdminPublishStaticPageR
trashRoute :: ContentKind -> ContentId -> Route App
trashRoute Article = AdminTrashArticleR
trashRoute Page = AdminTrashStaticPageR
msgContentSingle :: ContentKind -> AppMessage
msgContentSingle Article = MsgArticle
msgContentSingle Page = MsgPage
msgContentPlural :: ContentKind -> AppMessage
msgContentPlural Article = MsgArticles
msgContentPlural Page = MsgPages
msgNoContent :: ContentKind -> AppMessage
msgNoContent Article = MsgNoArticles
msgNoContent Page = MsgNoPages

-- | Markdown cheatsheet modal
markdownCheatsheet :: Widget
markdownCheatsheet = $(widgetFile "admin/markdown-cheatsheet")

-- | Fetch all content with their author information
pullContent :: (EI.From query expr backend (expr (Entity Content)),
                EI.From query expr backend (expr (Entity User)))
                => ContentKind
                -> Bool
                -> query (expr (Entity Content), expr (Entity User))
pullContent kind trash = E.from $ \(c, u) -> do
    E.where_ (c E.^. ContentAuthor E.==. u E.^. UserId
        E.&&. c E.^. ContentType E.==. E.val kind
        E.&&. c E.^. ContentTrash E.==. E.val trash)
    E.orderBy [E.desc (c E.^. ContentAdded)]
    return (c, u)

-- | General function for showing content based on type
showContent :: ContentKind -> AppMessage -> Handler Html
showContent kind titleMsg = do
    let isTrashRoute = False
    msgRender' <- getMessageRender
    let msgRender msg = msgRender' $ msg kind
    contentList <- runDB $ E.select $ pullContent kind False
    adminLayout $ do
        setTitleI titleMsg
        toWidget [lucius|
            #navigation .navigation-#{showToLower kind}s { background: red; }
            #navigation .navigation-new-#{showToLower kind} { display: block !important; }
            #navigation .navigation-trash-#{showToLower kind}s { display: block !important; }
        |]
        $(widgetFile "admin/list-content")

-- | View all trashed content
showTrashContent :: ContentKind -> AppMessage -> Handler Html
showTrashContent kind titleMsg = do
    msgRender' <- getMessageRender
    let msgRender msg = msgRender' $ msg kind
    let isTrashRoute = True
    contentList <- runDB $ E.select $ pullContent kind True
    adminLayout $ do
        setTitleI titleMsg
        toWidget [lucius|
            #navigation .navigation-new-#{showToLower kind} { display: block !important; }
            #navigation .navigation-trash-#{showToLower kind}s { background: red !important; display: block !important; }
        |]
        $(widgetFile "admin/list-content")

-- | Mark a piece of content as trashed
postTrashContent :: (Text -> AppMessage) -> Route App -> ContentId -> Handler Html
postTrashContent deletedMsg showRoute contentId = do
    runDB $ update contentId [ContentTrash =. True]
    content <- runDB $ get404 contentId
    setMessageI $ deletedMsg $ contentTitle content
    redirect showRoute

-- | Change the status of some content to unpublished
postUnpublishContent :: (Text -> AppMessage) -> Route App -> ContentId -> Handler Html
postUnpublishContent unpublishedMsg showRoute contentId = do
    runDB $ update contentId [ContentVisible =. False]
    content <- runDB $ get404 contentId
    setMessageI $ unpublishedMsg $ contentTitle content
    redirect showRoute

-- | Change the status of some content to published
postPublishContent :: (Text -> AppMessage) -> Route App -> ContentId -> Handler Html
postPublishContent publishedMsg showRoute contentId = do
    updated <- liftIO getCurrentTime
    runDB $ update contentId [ContentVisible =. True, ContentAdded =. updated]
    content <- runDB $ get404 contentId
    setMessageI $ publishedMsg $ contentTitle content
    redirect showRoute

-- | Form page for creating new content
getNewContent :: ContentKind -> AppMessage -> Route App -> Handler Html
getNewContent kind titleMsg formroute = do
    let mcontent = Nothing
    adminLayout $ do
        addScript $ StaticR js_showdown_js
        addScript $ StaticR js_extensions_github_js
        setTitleI titleMsg
        toWidget [lucius|
            #navigation .navigation-new-#{showToLower kind} { background: red !important; display: block !important; }
            #navigation .navigation-trash-#{showToLower kind}s { display: block !important; }
        |]
        $(widgetFile "admin/create-content")

-- | Handling the form for creating content
postNewContent :: ContentKind -> (Text -> AppMessage) -> (Text -> AppMessage)
                  -> (Text -> AppMessage) -> (ContentId -> Route App) -> Route App
                  -> Handler Html
postNewContent kind gistErrorMsg createdMsg savedMsg updatedRoute showRoute = do
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
            gistIdent <- maybeCreateOrUpdateGist Nothing "" title mdContent gistErrorMsg
            _ <- runDB $ insert $ Content kind title mdContent htmlContent added userId wordCount gistIdent True False
            setMessageI $ createdMsg title
            redirect showRoute
        Just _ -> do
            contentId <- runDB $ insert $ Content kind title mdContent htmlContent added userId wordCount Nothing False False
            setMessageI $ savedMsg title
            redirect $ updatedRoute contentId

-- | Form page for updating content
getUpdateContent :: ContentKind -> AppMessage -> (ContentId -> Route App)
                    -> ContentId -> Handler Html
getUpdateContent kind titleMsg updatedRoute contentId = do
    let formroute = updatedRoute contentId
    dbcontent <- runDB $ get404 contentId
    let mcontent = Just dbcontent
    adminLayout $ do
        addScript $ StaticR js_showdown_js
        addScript $ StaticR js_extensions_github_js
        setTitleI titleMsg
        toWidget [lucius|
            #navigation .navigation-new-#{showToLower kind} { display: block !important; }
            #navigation .navigation-trash-#{showToLower kind}s { display: block !important; }
        |]
        $(widgetFile "admin/create-content")

-- | Handling the form for updating content
postUpdateContent :: (Text -> AppMessage) -> (Text -> AppMessage)
                     -> (Text -> AppMessage) -> (Text -> AppMessage) ->
                     (ContentId -> Route App) -> Route App -> ContentId -> Handler Html
postUpdateContent gistErrorMsg savedMsg pubMsg unpubMsg updatedRoute showRoute contentId = do
    title <- runInputPost $ ireq textField "form-title-field"
    mdContent <- runInputPost $ ireq htmlField "form-mdcontent-field"
    htmlContent <- runInputPost $ ireq htmlField "form-htmlcontent-field"
    wordCount <- runInputPost $ ireq intField "form-wordcount-field"
    saved <- runInputPost $ iopt boolField "form-saved"
    unpublish <- runInputPost $ iopt boolField "form-unpublish"
    updated <- liftIO getCurrentTime
    -- Create or update the gist
    original <- runDB $ get404 contentId
    gistIdent <- maybeCreateOrUpdateGist (contentGistId original) (contentTitle original) title mdContent gistErrorMsg
    -- Handle changing the visible status and redirecting to the appropriate page
    case unpublish of
        Nothing -> do
            setMessageI $ pubMsg title
            wasSaved True saved showRoute title gistIdent mdContent htmlContent wordCount updated
        Just _ -> do
            setMessageI $ unpubMsg title
            wasSaved False saved (updatedRoute contentId) title gistIdent mdContent htmlContent wordCount (contentAdded original)
    where
        wasSaved publish saved redirectRoute t g mC hC wC updated =
            case saved of
                Nothing -> do
                    runDB $ update contentId [ ContentGistId =. g
                                             , ContentVisible =. publish
                                             , ContentTitle =. t
                                             , ContentMdContent =. mC
                                             , ContentHtmlContent =. hC
                                             , ContentWordCount =. wC
                                             , ContentAdded =. updated ]
                    redirect redirectRoute
                Just _ -> do
                    runDB $ update contentId [ ContentGistId =. g
                                             , ContentTitle =. t
                                             , ContentMdContent =. mC
                                             , ContentHtmlContent =. hC
                                             , ContentWordCount =. wC ]
                    setMessageI $ savedMsg t
                    redirect (updatedRoute contentId)
