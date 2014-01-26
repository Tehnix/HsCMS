{-# LANGUAGE TupleSections, OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
-- | Simple access to the GitHub gist API, allowing creation and updating of gists.
module Gist (
      GistContent
    , Gist
    , GistResponse
    , createGist
    , updateGist
  ) where

import           Import
import           Data.Monoid ((<>))
import           Data.Text (Text, unpack, pack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Conduit
import           Data.Conduit
import           Control.Monad.IO.Class
import           Data.Aeson
import           Control.Applicative
import           Control.Monad
import           GHC.Generics
import           Data.HashMap.Strict


-- | Container for the gist data.
data GistContent = GistContent
    { filename :: Maybe Text -- ^ The new filename of the gist. Only used with 'updateGist'.
    , content :: Text        -- ^ The content of the gist.
    } deriving (Show, Generic)

instance ToJSON GistContent

-- | Container for the gist.
data Gist = Gist
    { description :: Text -- ^ The description of the gist.
    , public      :: Bool -- ^ Only used with 'createGist'. If set to 'False' the gist will be secret, else it will be public.
    , files       :: HashMap Text GistContent -- ^ The filename of the gist and its content.
    } deriving (Show, Generic)

instance ToJSON Gist

-- | Container for the response returned from the GitHub API.
data GistResponse = GistResponse 
    { gistId :: Text -- ^ The id of the created/updated gist.
    } deriving (Show, Generic)

instance FromJSON GistResponse where
    parseJSON (Object v) = GistResponse <$> v .: "id"
    parseJSON _ = mzero

-- | Convert a lazy 'BL.ByteString' to a strict 'BS.ByteString'.
toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

{-|
  'submitPostRequest' sends the POST request to the url parameter, and return the response as a 'BL.ByteString' wrapped in 'MonadIO' or 'MonadBaseControl IO'.
-}
submitPostRequest :: (MonadIO m, MonadBaseControl IO m) => String -> Text -> BL.ByteString -> m BL.ByteString
submitPostRequest urlString githubKey body =
    case parseUrl urlString of
        Nothing -> return $ "URL Syntax Error"
        Just initReq -> withManager $ \manager -> do
            let req = initReq { secure = True
                              , method = "POST"
                              , requestHeaders = [("x-oauth-basic", (encodeUtf8 githubKey))]
                                                 <> [("User-Agent", "HsCMS")]
                              , requestBody = RequestBodyBS (toStrict body)
                              , checkStatus = \_ _ _ -> Nothing
                              }
            res <- httpLbs req manager
            return $ responseBody res

{-|
  The 'createGist' function sends a POST request to the 'https://api.github.com/gists' with the description, title and content as a JSON encoded body, and returns the id of the gist that was created.
  If the github key supplied is 'Nothing' then it will post an anonymous gist. If there is a value, it'll use that token to tie the gist to a specific user (it assumes the github key token has gist scope).
-}
createGist :: (MonadIO m, MonadBaseControl IO m) => Maybe Text -> Text -> Text -> Text -> m (Maybe GistResponse)
createGist githubKey desc title con = do
    case githubKey of
        Nothing -> return Nothing
        Just gkey -> do
            let body = encode $ toJSON $ Gist desc True (fromList [(title, GistContent Nothing con)])
            res <- submitPostRequest "https://api.github.com/gists" gkey body
            case (eitherDecode res) of
                Left _ -> return Nothing
                Right gist -> return $ Just gist


{-|
  The 'updateGist' function sends a POST request to the 'https://api.github.com/gists' with the description, the new title and old title (allowing title change) and content as a JSON encoded body, and returns the id of the gist that was updated.
  Unlike 'createGist', this requires that a github key is supplied since you can't update an anonymous gist.
-}
updateGist :: (MonadIO m, MonadBaseControl IO m) => Text -> Text -> Text -> Text -> Text -> Text -> m (Maybe GistResponse)
updateGist githubKey gId desc oldTitle newTitle con = do
    let body = encode $ toJSON $ Gist desc True (fromList [(oldTitle, GistContent (Just newTitle) con)])
    res <- submitPostRequest ("https://api.github.com/gists/" ++ (unpack gId)) githubKey body
    case (eitherDecode res) of
        Left _ -> return Nothing
        Right gist -> return $ Just gist
