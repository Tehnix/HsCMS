module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import qualified Data.Text as T

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        (title', parents) <- breadcrumbs

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheetRemote "/static/css/fonts.css"
            addScriptRemote "/static/js/jquery.js"
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")
        
    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR
    
    -- Require admin priviliges
    isAuthorized AdminR _ = return isAdmin
    isAuthorized AdminBlogR _ = return isAdmin
    isAuthorized AdminBlogNewR _ = return isAdmin
    
    -- Anyone can access all other pages
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = BlogR
    -- Where to send a user after logout
    logoutDest _ = BlogR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authGoogleEmail]

    authHttpManager = httpManager
    
    -- Overwrite the login handler
    loginHandler = loginLayout $ do
        $(widgetFile "login")


instance YesodBreadcrumbs App where
    -- Front-end breadcrumbs
    breadcrumb BlogR = return ("Home", Nothing)
    breadcrumb AboutR = return ("About", Just BlogR)
    breadcrumb (AuthorR authorName) = do
        crumb <- return $ T.pack $ "Author: " ++ (T.unpack authorName)
        return (crumb, Just BlogR)
    breadcrumb ArchivesR = return ("Archives", Just BlogR)
    breadcrumb (ArticleR articleId) = do
        article <- runDB $ get404 articleId
        return (articleTitle article, Just BlogR)
    
    -- Admin panel breadcrumbs
    breadcrumb AdminR = return ("Admin", Nothing)
    breadcrumb AdminBlogR = return ("Admin Blog", Just AdminR)
    breadcrumb AdminBlogNewR = return ("Admin Blog: New Post", Just AdminR)
    breadcrumb (AdminBlogPostR articleId) = do
        article <- runDB $ get404 articleId
        crumb <- return $ T.pack $ "Admin Blog: " ++ (T.unpack (articleTitle article))
        return (crumb, Just AdminR)
    breadcrumb (AdminBlogDeleteR _) = do
        return ("Delete", Just BlogR)
    
    -- These pages never call breadcrumb
    breadcrumb FaviconR = return ("", Nothing)
    breadcrumb StaticR{} = return ("", Nothing)
    breadcrumb RobotsR = return ("", Nothing)
    breadcrumb AuthR{} = return ("", Nothing)

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- The layout for the login page
loginLayout :: GWidget Auth App () -> GHandler Auth App RepHtml
loginLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    (title', parents) <- breadcrumbs
    pc <- widgetToPageContent $ do
        $(widgetFile "normalize")
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheetRemote "/static/css/fonts.css"
        addScriptRemote "/static/js/jquery.js"
        $(widgetFile "login-layout")
    hamletToRepHtml $(hamletFile "templates/login-layout-wrapper.hamlet")

-- TODO: Use the admins value in settings.yml instead
-- admins :: [T.Text]
-- admins = ["christianlaustsen@gmail.com"]

-- Check if a user is one of the pre-specified admin users
-- isAdmin :: Maybe (Entity (UserGeneric backend)) -> AuthResult
-- isAdmin (Just (Entity _ user)) | elem (userIdent user) admins = Authorized
--                                | otherwise                    = AuthenticationRequired
-- isAdmin Nothing = AuthenticationRequired

--isAdmin :: User -> AuthResult
isAdmin = do
  extra <- getExtra
  mauth <- maybeAuth
  case mauth of
      Nothing -> return AuthenticationRequired
      Just (Entity _ user) 
          | userIdent user `elem` extraAdmins extra -> return Authorized
          | otherwise                               -> return AuthenticationRequired
-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
