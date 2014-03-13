{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Pages where

import Core.Import
import Yesod.Default.Config (appExtra)
import qualified Database.Esqueleto as E


-- | Fetch all articles with their author information
pullStaticPages :: Handler [(Entity Content, Entity User)]
pullStaticPages = runDB $ E.select $
    E.from $ \(c, u) -> do
    E.where_ (c E.^. ContentAuthor E.==. u E.^. UserId
        E.&&. c E.^. ContentType E.==. E.val Page
        E.&&. c E.^. ContentVisible E.==. E.val True
        E.&&. c E.^. ContentTrash E.==. E.val False)
    E.orderBy [E.desc (c E.^. ContentAdded)]
    return (c, u)

-- | Display a single article
getStaticPageR :: ContentId -> Text -> Handler Html
getStaticPageR contentId _ = do
    master <- getYesod
    content <- runDB $ get404 contentId
    if not (contentVisible content)
        then notFound
        else defaultLayout $ do
            setTitle $ toHtml $ contentTitle content
            $(widgetFile "front/single-page")
