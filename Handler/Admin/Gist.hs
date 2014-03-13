{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Gist where

import Core.Import
import Handler.API.Gist
import Data.Text.Lazy (toStrict)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Maybe (fromMaybe)


-- | Create a gist.
maybeCreateGist :: (RenderMessage App msg) => Text -> Bool -> Text -> Html -> (Text -> msg) -> Handler (Maybe Text)
maybeCreateGist gToken public title mdContent errorMsg = do
    let content = toStrict (renderHtml mdContent)
    let auth = githubAuth gToken
    let gist = gistContent title public (title <> ".md") content
    res <- createGist (Just auth) gist
    case res of
        Nothing -> do
            setMessageI $ errorMsg title
            return Nothing
        Just gId -> return $ Just $ gistId gId

-- | Update a gist.
maybeUpdateGist :: (RenderMessage App msg) => Text -> Bool -> Text -> Text -> Text -> Html -> (Text -> msg) -> Handler (Maybe Text)
maybeUpdateGist gToken public existingGistId originalTitle title mdContent errorMsg = do
    let content = toStrict (renderHtml mdContent)
    let auth = githubAuth gToken
    let gist = gistUpdateContent title public (originalTitle <> ".md") (Just (title <> ".md")) content
    res <- updateGist auth existingGistId gist
    case res of
        Nothing -> do
            setMessageI $ errorMsg title
            return Nothing
        Just gId -> return $ Just $ gistId gId

-- | Update or create a new gist, depending on wheter there exists a gist id.
maybeCreateOrUpdateGist :: (RenderMessage App msg) => Maybe Text -> Text -> Text -> Html -> (Text -> msg) -> Handler (Maybe Text)
maybeCreateOrUpdateGist existingGistId originalTitle title mdContent errorMsg = do
    extra <- getExtra
    case extraGithubToken extra of
        Nothing -> return Nothing
        Just gToken -> do
            let public = fromMaybe True (extraGistPublic extra)
            case existingGistId of
                Nothing -> return =<< maybeCreateGist gToken public title mdContent errorMsg
                Just gId -> return =<< maybeUpdateGist gToken public gId originalTitle title mdContent errorMsg
