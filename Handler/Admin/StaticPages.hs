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


-- | Markdown cheatsheet modal
markdownCheatsheet :: Widget
markdownCheatsheet = $(widgetFile "admin/markdown-cheatsheet")

-- | Fetch all articles with their author information
pullStaticPages :: (EI.From query expr backend (expr (Entity StaticPage)), EI.From query expr backend (expr (Entity User))) => Bool -> query (expr (Entity StaticPage), expr (Entity User))
pullStaticPages trash = E.from $ \(a, u) -> do
    E.where_ (a E.^. StaticPageAuthor E.==. u E.^. UserId E.&&. a E.^. StaticPageTrash E.==. E.val trash)
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
    where
        contentTitle = staticPageTitle
        contentMarkdown = staticPageMdContent
        contentHtmlContent = staticPageHtmlContent
        contentVisible = staticPageVisible
        contentAdded = staticPageAdded
        contentWordCount = staticPageWordCount
        updateRoute = AdminUpdateStaticPageR
        viewRoute = StaticPageR
        unpublishRoute = AdminUnpublishStaticPageR
        publishRoute = AdminPublishStaticPageR
        trashRoute = AdminTrashStaticPageR
        msgContentSingle = MsgPage
        msgContentPlural = MsgPages
        msgNoContent = MsgNoPages

-- | Form page for creating a new article
getAdminNewStaticPageR :: Handler Html
getAdminNewStaticPageR = do
    formroute <- return $ AdminNewStaticPageR
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
    where
        contentTitle c = articleTitle c
        contentMarkdown c = articleMdContent c
        contentVisible c = articleVisible c

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
            setMessageI $ MsgMsgSavedStaticPage $ title
            redirect (AdminUpdateStaticPageR staticPageId)
        Just _ -> do
            setMessageI $ MsgMsgCreatedStaticPage $ title
            
            -- Create a gist of the post if the GitHub PA Token is set
            extra <- getExtra
            gistIdent <- case (extraGithubToken extra) of
                Nothing -> return Nothing
                Just gToken -> do
                    let mdCont = toStrict (renderHtml mdContent)
                    res <- createGist (Just (GitHubToken gToken)) $ Gist title (maybe True (\r -> r) (extraGistPublic extra)) $ fromList [(title <> ".md", (GistContent mdCont Nothing))]
                    case res of
                        Nothing -> do
                            setMessageI $ MsgMsgCreatedStaticPageGistError $ title
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
    where
        contentTitle = staticPageTitle
        contentMarkdown = staticPageMdContent
        contentVisible = staticPageVisible

-- | Handling the form for updating an article
postAdminUpdateStaticPageR :: StaticPageId -> Handler Html
postAdminUpdateStaticPageR pageId = do
    title <- runInputPost $ ireq textField "form-title-field"
    mdContent <- runInputPost $ ireq htmlField "form-mdcontent-field"
    htmlContent <- runInputPost $ ireq htmlField "form-htmlcontent-field"
    wordCount <- runInputPost $ ireq intField "form-wordcount-field"
    publish <- runInputPost $ iopt boolField "form-publish"
    unpublish <- runInputPost $ iopt boolField "form-unpublish"
    
    -- Update the gist of the post if the GitHub PA Token is set
    originalStaticPage <- runDB $ get404 pageId
    extra <- getExtra
    gistIdent <- case (extraGithubToken extra) of
        Nothing -> return Nothing
        Just gToken -> do
            let mdCont = toStrict (renderHtml mdContent)
            res <- case (staticPageGistId originalStaticPage) of
                Nothing -> do
                    -- If the article doesn't have a gist ID already
                    return =<< createGist (Just (GitHubToken gToken)) $ Gist title (maybe True (\r -> r) (extraGistPublic extra)) $ fromList [(title <> ".md", (GistContent mdCont Nothing))]
                Just gId -> do
                    return =<< updateGist (GitHubToken gToken) gId $ Gist title True $ fromList [((staticPageTitle originalStaticPage) <> ".md", (GistContent mdCont (Just (title <> ".md"))))]
            case res of
                Nothing -> return Nothing
                Just (GistResponse gId) -> return $ Just gId
    
    -- Set the Bool and message depending on whether the post is published or unpublished
    publishStatus <- case unpublish of
        Nothing -> do
            setMessageI $ MsgMsgPublishedStaticPage $ title
            return True
        Just _ -> do
            setMessageI $ MsgMsgUnpublishedStaticPage $ title
            return False
    
    -- Either save the post (ignoring if it's published or not), or change the publish status of the post
    case publish of
        Nothing -> do
            runDB $ update pageId [StaticPageGistId =. gistIdent, StaticPageVisible =. publishStatus, StaticPageTitle =. title, StaticPageMdContent =. mdContent, StaticPageHtmlContent =. htmlContent, StaticPageWordCount =. wordCount]
            if publishStatus then
                setMessageI $ MsgMsgSavedStaticPage $ title 
                else setMessageI $ MsgMsgUnpublishedStaticPage $ title
            redirect (AdminUpdateStaticPageR pageId)
        Just _ -> do
            runDB $ update pageId [StaticPageGistId =. gistIdent, StaticPageVisible =. publishStatus, StaticPageTitle =. title, StaticPageMdContent =. mdContent, StaticPageHtmlContent =. htmlContent, StaticPageWordCount =. wordCount]
            redirect AdminShowStaticPagesR

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
    where
        contentTitle = staticPageTitle
        contentMarkdown = staticPageMdContent
        contentHtmlContent = staticPageHtmlContent
        contentVisible = staticPageVisible
        contentAdded = staticPageAdded
        contentWordCount = staticPageWordCount
        updateRoute = AdminUpdateStaticPageR
        viewRoute = StaticPageR
        unpublishRoute = AdminUnpublishStaticPageR
        publishRoute = AdminPublishStaticPageR
        trashRoute = AdminTrashStaticPageR
        msgContentSingle = MsgPage
        msgContentPlural = MsgPages
        msgNoContent = MsgNoPages

-- | Mark an article as trashed
postAdminTrashStaticPageR :: StaticPageId -> Handler Html
postAdminTrashStaticPageR pageId = do
    runDB $ update pageId [StaticPageTrash =. True]
    page <- runDB $ get404 pageId
    setMessageI $ MsgMsgDeletedStaticPage $ (staticPageTitle page)
    redirect AdminShowStaticPagesR

-- | Change the status of an article to unpublished
postAdminUnpublishStaticPageR :: StaticPageId -> Handler Html
postAdminUnpublishStaticPageR pageId = do
    runDB $ update pageId [StaticPageVisible =. False]
    page <- runDB $ get404 pageId
    setMessageI $ MsgMsgUnpublishedStaticPage $ (staticPageTitle page)
    redirect AdminShowStaticPagesR

-- | Change the status of an article to published
postAdminPublishStaticPageR :: StaticPageId -> Handler Html
postAdminPublishStaticPageR pageId = do
    runDB $ update pageId [StaticPageVisible =. True]
    page <- runDB $ get404 pageId
    setMessageI $ MsgMsgPublishedStaticPage $ (staticPageTitle page)
    redirect AdminShowStaticPagesR

