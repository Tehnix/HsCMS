{-# LANGUAGE TupleSections, OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
{-|
  Simple access to some parts of the GitHub gist API, mainly creating and updating gists. It still requires you to create a Personal Access Token on GitHub by yourself (atleast if you want authentication to work).

  An example of creating an anonymous gist using 'createGist',

  @
  createGist Nothing $ Gist \"Description...\" True $ fromList [(\"File.md\", (GistContent \"Some Content!\" Nothing))]
  @

  And a gist tied to a user,
   
  @
  createGist (Just (GitHubToken \"The token\")) $ Gist \"Description...\" True $ fromList [(\"File.md\", (GistContent \"Some Content!\" Nothing))]
  @
-}
module Gist (
      createGist
    , updateGist
    , GitHubToken(..)
    , GistContent(..)
    , Gist(..)
    , GistResponse(..)
  ) where

import Prelude hiding (head, init, last, readFile, tail, writeFile)
import GHC.Generics (Generic)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.HashMap.Strict
import Data.Conduit (MonadBaseControl)
import Data.Monoid ((<>))
import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Types.Header (RequestHeaders)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>))
import Control.Monad (mzero)


-- | The GitHub Personal Access Token.
newtype GitHubToken = GitHubToken Text deriving (Show)

-- | The filename and file content of a gist.
data GistContent = GistContent
    { content :: Text        -- ^ The content of the gist.
    , filename :: Maybe Text -- ^ The new filename of the gist. Only used with 'updateGist'.
    } deriving (Show, Generic)

instance ToJSON GistContent

-- | The gist data and metadata.
data Gist = Gist
    { description :: Text -- ^ The description of the gist.
    , public      :: Bool -- ^ Only used with 'createGist'. If set to 'False' the gist will be secret, else it will be public.
    , files       :: HashMap Text GistContent -- ^ The filename of the gist and its content.
    } deriving (Show, Generic)

instance ToJSON Gist

-- | The response returned from the GitHub API.
data GistResponse = GistResponse 
    { gistId :: Text -- ^ The id of the created/updated gist.
    } deriving (Show, Generic)

instance FromJSON GistResponse where
    parseJSON (Object v) = GistResponse <$> v .: "id"
    parseJSON _ = mzero

-- | Convert the GitHub Personal Access Token to a basic authorization header
getTokenHeader :: GitHubToken -> RequestHeaders
getTokenHeader (GitHubToken tk) = [("Authorization", "token " <> (encodeUtf8 tk))]

{-|
  'submitPostRequest' sends the POST request to the url parameter, and return the response as a 'ByteString' wrapped in 'MonadIO' or 'MonadBaseControl IO'.
-}
submitPostRequest :: (MonadIO m, MonadBaseControl IO m) => String -> Maybe GitHubToken -> ByteString -> m ByteString
submitPostRequest urlString githubToken body = do
    let tokenHeader = maybe [] getTokenHeader githubToken
    case parseUrl urlString of
        Nothing -> return $ "URL Syntax Error"
        Just initReq -> withManager $ \manager -> do
            let req = initReq { secure = True
                              , method = "POST"
                              , requestHeaders = tokenHeader <> [("User-Agent", "HsCMS")]
                              , requestBody = RequestBodyBS (toStrict body)
                              , checkStatus = \_ _ _ -> Nothing
                              }
            res <- httpLbs req manager
            return $ responseBody res

{-|
  Sends a POST request to the https://api.github.com/gists with the gist as a JSON encoded body, and returns the id of the gist that was created.
  If the GitHub Personal Access Token supplied is 'Nothing' then it will post an anonymous gist. If there is a value, it'll use that token to tie the gist to a specific user (it assumes the GitHub token has gist scope).
-}
createGist :: (MonadIO m, MonadBaseControl IO m) => Maybe GitHubToken -> Gist -> m (Maybe GistResponse)
createGist githubToken gist = do
    let body = encode $ toJSON gist
    res <- submitPostRequest "https://api.github.com/gists" githubToken body
    case (eitherDecode res) of
        Left _ -> return Nothing
        Right gistResponse -> return $ Just gistResponse

{-|
  Sends a POST request to the https://api.github.com/gists with the gist as a JSON encoded body, and returns the id of the gist that was updated.
  Unlike 'createGist', this requires that a GitHub Personal Access Token is supplied since you can't update an anonymous gist.
-}
updateGist :: (MonadIO m, MonadBaseControl IO m) => GitHubToken -> Text -> Gist -> m (Maybe GistResponse)
updateGist githubToken gId gist = do
    let body = encode $ toJSON gist
    let url = "https://api.github.com/gists/" ++ (unpack gId)
    res <- submitPostRequest url (Just githubToken) body
    case (eitherDecode res) of
        Left _ -> return Nothing
        Right gistResponse -> return $ Just gistResponse
