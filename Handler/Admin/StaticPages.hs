{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.StaticPages (
      getAdminShowStaticPagesR
    , getAdminNewStaticPageR
    , postAdminNewStaticPageR
    , getAdminUpdateStaticPageR
    , postAdminUpdateStaticPageR
    , getAdminShowTrashStaticPagesR
    , postAdminTrashStaticPageR
    , postAdminUnpublishStaticPageR
    , postAdminPublishStaticPageR
  ) where

import           Core.Import
import           Yesod.Auth
import           System.Locale (defaultTimeLocale)
import           Data.Time
import           Data.Text.Lazy (toStrict)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Language as EI
import           Handler.API.Gist
import           Data.Maybe (fromMaybe)


{-|
  This block generalises the templates so they can be used with other
  content.
-}
contentTitle :: StaticPage -> Text
contentTitle = staticPageTitle
contentMarkdown :: StaticPage -> Html
contentMarkdown = staticPageMdContent
contentHtmlContent :: StaticPage -> Html
contentHtmlContent = staticPageHtmlContent
contentVisible :: StaticPage -> Bool
contentVisible = staticPageVisible
contentAdded :: StaticPage -> UTCTime
contentAdded = staticPageAdded
contentWordCount :: StaticPage -> Int
contentWordCount = staticPageWordCount
updateRoute :: StaticPageId -> Route App
updateRoute = AdminUpdateStaticPageR
viewRoute :: StaticPageId -> Text -> Route App
viewRoute = StaticPageR
unpublishRoute :: StaticPageId -> Route App
unpublishRoute = AdminUnpublishStaticPageR
publishRoute :: StaticPageId -> Route App
publishRoute = AdminPublishStaticPageR
trashRoute :: StaticPageId -> Route App
trashRoute = AdminTrashStaticPageR
msgContentSingle :: AppMessage
msgContentSingle = MsgPage
msgContentPlural :: AppMessage
msgContentPlural = MsgPages
msgNoContent :: AppMessage
msgNoContent = MsgNoPages

-- | Markdown cheatsheet modal
markdownCheatsheet :: Widget
markdownCheatsheet = $(widgetFile "admin/markdown-cheatsheet")

-- | Fetch all articles with their author information
pullStaticPages :: (EI.From query expr backend (expr (Entity StaticPage)), 
                    EI.From query expr backend (expr (Entity User))) => 
                    Bool -> query (expr (Entity StaticPage), expr (Entity User))
pullStaticPages trash = E.from $ \(a, u) -> do
    E.where_ (a E.^. StaticPageAuthor E.==. u E.^. UserId 
        E.&&. a E.^. StaticPageTrash E.==. E.val trash)
    E.orderBy [E.desc (a E.^. StaticPageAdded)]
    return (a, u)

-- | View all articles
getAdminShowStaticPagesR :: Handler Html
getAdminShowStaticPagesR = do
    msgRender <- getMessageRender
    let isTrashRoute = False
    contentList <- runDB $ E.select $ pullStaticPages False
    adminLayout $ do
        setTitleI MsgTitleAdminStaticPages
        toWidget [lucius| 
            #navigation .navigation-pages { background: red; }
            #navigation .navigation-new-page { display: block !important; } 
            #navigation .navigation-trash-pages { display: block !important; } 
        |]
        $(widgetFile "admin/list-content")

-- | Form page for creating a new article
getAdminNewStaticPageR :: Handler Html
getAdminNewStaticPageR = do
    formroute <- return AdminNewStaticPageR
    mcontent <- return Nothing
    adminLayout $ do
        addScript $ StaticR js_showdown_js
        addScript $ StaticR js_extensions_github_js
        setTitleI MsgTitleAdminNewStaticPage
        toWidget [lucius| 
            #navigation .navigation-new-page { background: red !important; display: block !important; } 
            #navigation .navigation-trash-pages { display: block !important; } 
        |]
        $(widgetFile "admin/create-content")

-- | Handling the form for creating an article
postAdminNewStaticPageR :: Handler Html
postAdminNewStaticPageR = do
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
            staticPageId <- runDB $ insert $ StaticPage title mdContent htmlContent wordCount added userId Nothing False False
            setMessageI $ MsgMsgSavedStaticPage title
            redirect (AdminUpdateStaticPageR staticPageId)
        Just _ -> do
            setMessageI $ MsgMsgCreatedStaticPage title
            -- Create a gist of the post if the GitHub PA Token is set
            extra <- getExtra
            gistIdent <- case extraGithubToken extra of
                Nothing -> return Nothing
                Just gToken -> do
                    let mdCont = toStrict (renderHtml mdContent)
                    let auth = githubAuth gToken
                    let gist = gistContent title (fromMaybe True (extraGistPublic extra)) title mdCont
                    res <- createGist (Just auth) gist
                    case res of
                        Nothing -> do
                            setMessageI $ MsgMsgCreatedStaticPageGistError title
                            return Nothing
                        Just (GistResponse gId) -> return $ Just gId
            
            _ <- runDB $ insert $ StaticPage title mdContent htmlContent wordCount added userId gistIdent True False
            redirect AdminShowStaticPagesR

-- | Form page for updating an article
getAdminUpdateStaticPageR :: StaticPageId -> Handler Html
getAdminUpdateStaticPageR pageId = do
    formroute <- return $ AdminUpdateStaticPageR pageId
    dbcontent <- runDB $ get404 pageId
    mcontent <- return $ Just dbcontent
    adminLayout $ do
        addScript $ StaticR js_showdown_js
        addScript $ StaticR js_extensions_github_js
        setTitleI MsgTitleAdminUpdateStaticPage
        toWidget [lucius| 
            #navigation .navigation-new-page { display: block !important; } 
            #navigation .navigation-trash-pages { display: block !important; } 
        |]
        $(widgetFile "admin/create-content")

-- | Handling the form for updating an article
postAdminUpdateStaticPageR :: StaticPageId -> Handler Html
postAdminUpdateStaticPageR pageId = do
    title <- runInputPost $ ireq textField "form-title-field"
    mdContent <- runInputPost $ ireq htmlField "form-mdcontent-field"
    htmlContent <- runInputPost $ ireq htmlField "form-htmlcontent-field"
    wordCount <- runInputPost $ ireq intField "form-wordcount-field"
    saved <- runInputPost $ iopt boolField "form-saved"
    unpublish <- runInputPost $ iopt boolField "form-unpublish"
    updated <- liftIO getCurrentTime
    -- Update the gist of the post if the GitHub PA Token is set
    original <- runDB $ get404 pageId
    extra <- getExtra
    gistIdent <- case extraGithubToken extra of
        Nothing -> return Nothing
        Just gToken -> do
            let mdCont = toStrict (renderHtml mdContent)
            let auth = githubAuth gToken
            res <- case staticPageGistId original of
                -- If the article doesn't have a gist ID already
                Nothing -> do
                    let gist = gistContent title (fromMaybe True (extraGistPublic extra)) title mdCont
                    return =<< createGist (Just auth) gist
                Just gId -> do
                    let gist = gistUpdateContent title (fromMaybe True (extraGistPublic extra)) (staticPageTitle original <> ".md") (Just title) mdCont
                    return =<< updateGist auth gId gist
            case res of
                Nothing -> return Nothing
                Just (GistResponse gId) -> return $ Just gId
    -- Handle changing the visible status and redirecting to the appropriate page
    case unpublish of
        Nothing -> do
            setMessageI $ MsgMsgPublishedStaticPage title
            wasSaved True saved AdminShowStaticPagesR title gistIdent mdContent htmlContent wordCount updated
        Just _ -> do
            setMessageI $ MsgMsgUnpublishedStaticPage title
            wasSaved False saved (AdminUpdateStaticPageR pageId) title gistIdent mdContent htmlContent wordCount (staticPageAdded original)
    where
        wasSaved publish saved directTo t g mC hC wC updated = 
            case saved of
                Nothing -> do
                    runDB $ update pageId [ StaticPageGistId =. g
                                             , StaticPageVisible =. publish
                                             , StaticPageTitle =. t
                                             , StaticPageMdContent =. mC
                                             , StaticPageHtmlContent =. hC
                                             , StaticPageWordCount =. wC
                                             , StaticPageAdded =. updated ]
                    redirect directTo
                Just _ -> do
                    runDB $ update pageId [ StaticPageGistId =. g
                                             , StaticPageTitle =. t
                                             , StaticPageMdContent =. mC
                                             , StaticPageHtmlContent =. hC
                                             , StaticPageWordCount =. wC ]
                    setMessageI $ MsgMsgSavedStaticPage t
                    redirect (AdminUpdateStaticPageR pageId)

-- | View all trashed articles
getAdminShowTrashStaticPagesR :: Handler Html
getAdminShowTrashStaticPagesR = do
    msgRender <- getMessageRender
    let isTrashRoute = True
    contentList <- runDB $ E.select $ pullStaticPages True
    adminLayout $ do
        setTitleI MsgTitleAdminTrashStaticPages
        toWidget [lucius| 
            #navigation .navigation-new-page { display: block !important; } 
            #navigation .navigation-trash-pages { background: red !important; display: block !important; } 
        |]
        $(widgetFile "admin/list-content")

-- | Mark an article as trashed
postAdminTrashStaticPageR :: StaticPageId -> Handler Html
postAdminTrashStaticPageR pageId = do
    runDB $ update pageId [StaticPageTrash =. True]
    page <- runDB $ get404 pageId
    setMessageI $ MsgMsgDeletedStaticPage $ staticPageTitle page
    redirect AdminShowStaticPagesR

-- | Change the status of an article to unpublished
postAdminUnpublishStaticPageR :: StaticPageId -> Handler Html
postAdminUnpublishStaticPageR pageId = do
    runDB $ update pageId [StaticPageVisible =. False]
    page <- runDB $ get404 pageId
    setMessageI $ MsgMsgUnpublishedStaticPage $ staticPageTitle page
    redirect AdminShowStaticPagesR

-- | Change the status of an article to published
postAdminPublishStaticPageR :: StaticPageId -> Handler Html
postAdminPublishStaticPageR pageId = do
    updated <- liftIO getCurrentTime
    runDB $ update pageId [StaticPageVisible =. True, StaticPageAdded =. updated]
    page <- runDB $ get404 pageId
    setMessageI $ MsgMsgPublishedStaticPage $ staticPageTitle page
    redirect AdminShowStaticPagesR

