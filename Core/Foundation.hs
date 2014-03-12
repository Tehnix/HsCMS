{-# LANGUAGE OverloadedStrings     #-}
module Core.Foundation where

import Prelude hiding (takeWhile)
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Core.Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Core.Settings (widgetFile, Extra (..))
import Core.Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)

-- Custom imports
import Core.HelperFunctions
import Core.Layout
import Data.Text (Text, takeWhile, unpack)
import Data.Monoid ((<>))
import Data.Maybe

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Core.Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Core.Settings.PersistConf
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        extra <- getExtra
        mmsg <- getMessage
        (title', parents) <- breadcrumbs
        let layout = unpack $ fromMaybe "default-layout" $ extraLayout extra

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                , css_fonts_css
                ])
            $(combineScripts 'StaticR
                [ js_jquery_js
                ])
            addScriptRemote "https://google-code-prettify.googlecode.com/svn/loader/run_prettify.js"
            getItem layout $(getWidgets)
        giveUrlRenderer $ getItem layout $(getWrappers)

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Core.Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR Yesod.Auth.GoogleEmail.forwardUrl

    -- Require admin priviliges
    isAuthorized AdminDashboardR _ = isAdmin
    isAuthorized AdminShowArticlesR _ = isAdmin
    isAuthorized AdminNewArticleR _ = isAdmin
    isAuthorized AdminShowTrashArticlesR _ = isAdmin
    isAuthorized (AdminUpdateArticleR _) _ = isAdmin
    isAuthorized (AdminTrashArticleR _) _ = isAdmin
    isAuthorized (AdminPublishArticleR _) _ = isAdmin
    isAuthorized (AdminUnpublishArticleR _) _ = isAdmin
    -- Anyone can access all other pages
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Core.Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId
    -- Where to send a user after successful login
    loginDest _ = AdminDashboardR
    -- Where to send a user after logout
    logoutDest _ = ArticlesR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> fmap Just $ insert User
                    { userIdent = credsIdent creds
                    , userPassword = Nothing
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authGoogleEmail]

    authHttpManager = httpManager

instance YesodBreadcrumbs App where
    -- Front-end breadcrumbs
    breadcrumb ArticlesR = return ("Home", Nothing)
    breadcrumb AboutR = return ("About", Just ArticlesR)
    breadcrumb (AuthorR authorId) = do
        user <- runDB $ get404 authorId
        let crumb = "Author: " <> takeWhile (/='@') (userIdent user)
        return (crumb, Just ArticlesR)
    breadcrumb ArchivesR = return ("Archives", Just ArticlesR)
    breadcrumb (ArticleR articleId _) = do
        article <- runDB $ get404 articleId
        return (articleTitle article, Just ArticlesR)
    breadcrumb (StaticPageR staticPageId _) = do
        staticPage <- runDB $ get404 staticPageId
        return (staticPageTitle staticPage, Just ArticlesR)

    -- Admin panel breadcrumbs
    breadcrumb AdminDashboardR = do
        render <- getMessageRender
        return (render MsgCrumbAdminDashboard, Nothing)
    -- Admin panel Articles breadcrumbs
    breadcrumb AdminShowArticlesR = do
        render <- getMessageRender
        return (render MsgCrumbAdminArticles, Just AdminDashboardR)
    breadcrumb AdminShowTrashArticlesR = do
        render <- getMessageRender
        return (render MsgCrumbAdminTrashedArticles, Just AdminDashboardR)
    breadcrumb AdminNewArticleR = do
        render <- getMessageRender
        return (render MsgCrumbAdminNewArticle, Just AdminDashboardR)
    breadcrumb (AdminUpdateArticleR articleId) = do
        render <- getMessageRender
        article <- runDB $ get404 articleId
        let crumb = render $ MsgCrumbAdminUpdated (articleTitle article)
        return (crumb, Just AdminShowArticlesR)
    breadcrumb (AdminTrashArticleR articleId) = do
        render <- getMessageRender
        article <- runDB $ get404 articleId
        let crumb = render $ MsgCrumbAdminTrashed (articleTitle article)
        return (crumb, Just AdminShowTrashArticlesR)
    breadcrumb (AdminUnpublishArticleR articleId) = do
        render <- getMessageRender
        article <- runDB $ get404 articleId
        let crumb = render $ MsgCrumbAdminUnpublished (articleTitle article)
        return (crumb, Just AdminShowArticlesR)
    breadcrumb (AdminPublishArticleR articleId) = do
        render <- getMessageRender
        article <- runDB $ get404 articleId
        let crumb = render $ MsgCrumbAdminPublished (articleTitle article)
        return (crumb, Just AdminShowArticlesR)
    -- Admin panel Static Pages breadcrumbs
    breadcrumb AdminShowStaticPagesR = do
        render <- getMessageRender
        return (render MsgCrumbAdminStaticPages, Just AdminDashboardR)
    breadcrumb AdminShowTrashStaticPagesR = do
        render <- getMessageRender
        return (render MsgCrumbAdminTrashedStaticPages, Just AdminDashboardR)
    breadcrumb AdminNewStaticPageR = do
        render <- getMessageRender
        return (render MsgCrumbAdminNewStaticPage, Just AdminDashboardR)
    breadcrumb (AdminUpdateStaticPageR staticPageId) = do
        render <- getMessageRender
        staticPage <- runDB $ get404 staticPageId
        let crumb = render $ MsgCrumbAdminUpdated (staticPageTitle staticPage)
        return (crumb, Just AdminShowStaticPagesR)
    breadcrumb (AdminTrashStaticPageR staticPageId) = do
        render <- getMessageRender
        staticPage <- runDB $ get404 staticPageId
        let crumb = render $ MsgCrumbAdminTrashed (staticPageTitle staticPage)
        return (crumb, Just AdminShowTrashStaticPagesR)
    breadcrumb (AdminUnpublishStaticPageR staticPageId) = do
        render <- getMessageRender
        staticPage <- runDB $ get404 staticPageId
        let crumb = render $ MsgCrumbAdminUnpublished (staticPageTitle staticPage)
        return (crumb, Just AdminShowStaticPagesR)
    breadcrumb (AdminPublishStaticPageR staticPageId) = do
        render <- getMessageRender
        staticPage <- runDB $ get404 staticPageId
        let crumb = render $ MsgCrumbAdminPublished (staticPageTitle staticPage)
        return (crumb, Just AdminShowStaticPagesR)

    -- These pages never call breadcrumb
    breadcrumb FaviconR = return ("", Nothing)
    breadcrumb StaticR{} = return ("", Nothing)
    breadcrumb RobotsR = return ("", Nothing)
    breadcrumb AuthR{} = return ("", Nothing)
    breadcrumb AdminCloudFlareStatsR = return ("", Nothing)
    breadcrumb AdminDisqusStatsR = return ("", Nothing)

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

-- | Check if a users email is present in the admins list in settings.
isAdmin :: Handler AuthResult
isAdmin = do
  extra <- getExtra
  mauth <- maybeAuth
  case mauth of
      Nothing -> return AuthenticationRequired
      Just (Entity _ user)
          | userIdent user `elem` extraAdmins extra -> return Authorized
          | otherwise                               -> return AuthenticationRequired

-- | The layout for the admin panel.
adminLayout :: Widget -> Handler Html
adminLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    (title', parents) <- breadcrumbs
    userEmail <- fmap usersEmail maybeAuth
    emailHash <- fmap lowerEmailHash maybeAuth
    pc <- widgetToPageContent $ do
        $(combineStylesheets 'StaticR
            [ css_normalize_css
            , css_bootstrap_css
            , css_fonts_css
            ])
        $(combineScripts 'StaticR
            [ js_jquery_js
            , js_bootstrap_min_js
            ])
        $(widgetFile "admin/layout")
    giveUrlRenderer $(hamletFile "templates/admin/layout-wrapper.hamlet")
