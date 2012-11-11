module Handler.Content (getContentR, postContentR, getContentPageR) where

import Import
import Yesod.Auth

-- to use Html into forms
import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

isAdmin :: User -> Bool
isAdmin user = userIdent user == "christianlaustsen@gmail.com"

entryForm :: Form ContentPage
entryForm = renderDivs $ ContentPage
    <$> areq textField "Title" Nothing
    <*> areq nicHtmlField "Content" Nothing

-- The view showing the list of pages
getContentR :: Handler RepHtml
getContentR = do
    maid <- maybeAuthId
    muser <- maybeAuth
    -- Get the list of pages inside the database
    contentPages <- runDB $ selectList [] [Desc ContentPageTitle]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/pages.hamlet)
    (pageWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "pages")

postContentR :: Handler RepHtml
postContentR = do
    ((res, pageWidget), enctype) <- runFormPost entryForm
    case res of
        FormSuccess contentPage -> do
            contentPageId <- runDB $ insert contentPage
            setMessage $ toHtml $ (contentPageTitle contentPage) <> " created"
            redirect $ ContentPageR contentPageId
        _ -> defaultLayout $ do
            setTitle "Please correct your entry form"
            $(widgetFile "pageAddError")

getContentPageR :: ContentPageId -> Handler RepHtml
getContentPageR contentPageId = do
    contentPage <- runDB $ get404 contentPageId
    defaultLayout $ do
        setTitle $ toHtml $ contentPageTitle contentPage
        $(widgetFile "page")

