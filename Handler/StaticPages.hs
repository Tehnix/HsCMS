{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.StaticPages where

import Import
import Yesod.Default.Config (appExtra)
import qualified Database.Esqueleto as E


-- | Fetch all articles with their author information
pullStaticPages :: Handler [(Entity StaticPage, Entity User)]
pullStaticPages = runDB $ E.select $
    E.from $ \(a, u) -> do
    E.where_ (a E.^. StaticPageAuthor E.==. u E.^. UserId E.&&. a E.^. StaticPageVisible E.==. E.val True E.&&. a E.^. StaticPageTrash E.==. E.val False)
    E.orderBy [E.desc (a E.^. StaticPageAdded)]
    return (a, u)

-- | Display a single article
getStaticPageR :: StaticPageId -> Text -> Handler Html
getStaticPageR staticPageId _ = do
    master <- getYesod
    staticPage <- runDB $ get404 staticPageId
    if not (staticPageVisible staticPage)
        then notFound
        else defaultLayout $ do
            setTitle $ toHtml $ staticPageTitle staticPage
            $(widgetFile "pages/single-page")

