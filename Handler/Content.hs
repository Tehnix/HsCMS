module Handler.Content 
    (getContentR
    , getContentPageR
) where

import Import
import Yesod.Auth

-- The view showing the list of pages
getContentR :: Handler RepHtml
getContentR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of pages inside the database
    contentPages <- runDB $ selectList [] [Desc ContentPageTitle]
    defaultLayout $ do
        $(widgetFile "pages")

-- The view show a specific page
getContentPageR :: ContentPageId -> Handler RepHtml
getContentPageR contentPageId = do
    contentPage <- runDB $ get404 contentPageId
    defaultLayout $ do
        setTitle $ toHtml $ contentPageTitle contentPage
        $(widgetFile "page")

