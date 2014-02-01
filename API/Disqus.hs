{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
module API.Disqus (
    getDisqusStats
  , DisqusRequest(..)
  , DisqusResponseData(..)
  , DisqusResponse(..)
  ) where

import Prelude hiding (head, init, last, readFile, tail, writeFile)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Data.ByteString.Char8 as C (unpack)
import Data.Conduit (MonadBaseControl)
import Data.Aeson
import Network.HTTP.Conduit hiding (queryString)
import Network.HTTP.QueryString (toString, queryString)
import Control.Monad.IO.Class (MonadIO, liftIO)


-- | 'DisqusAuth' consists of a secret API key and an access token.
data DisqusRequest = DisqusRequest 
    { secretKey   :: Text
    , accessToken :: Text
    , forum       :: Text
    } deriving (Show)

-- | The data from Disqus.
data DisqusResponseData = DisqusResponseData
    { title :: Text
    , posts :: Int
    , likes :: Int
    , link :: Text
    } deriving (Show, Generic)

instance ToJSON DisqusResponseData
instance FromJSON DisqusResponseData

-- | The response from Disqus.
data DisqusResponse = DisqusResponse 
    { code     :: Int
    , response :: [DisqusResponseData]
    } deriving (Show, Generic)

instance ToJSON DisqusResponse
instance FromJSON DisqusResponse

{-|
  'submitPostRequest' sends the POST request to the url parameter, and return the response as a 'L.ByteString' wrapped in 'MonadIO' or 'MonadBaseControl IO' monad.
-}
submitPostRequest :: (MonadIO m, MonadBaseControl IO m) => String -> m L.ByteString
submitPostRequest urlString = do
    case parseUrl urlString of
        Nothing -> return $ "URL Syntax Error"
        Just initReq -> withManager $ \manager -> do
            let req = initReq { secure = True
                              , requestHeaders = [("User-Agent", "HsCMS")]
                              , checkStatus = \_ _ _ -> Nothing
                              }
            res <- httpLbs req manager
            return $ responseBody res

{-|
  Sends a POST request to https://disqus.com/api/3.0/applications/listUsage.json with the the secret API key and access token as GET parameters, and returns a list of the popular articles.
-}
getDisqusStats :: (MonadIO m, MonadBaseControl IO m) => DisqusRequest -> m (Maybe DisqusResponse)
getDisqusStats (DisqusRequest s a f) = do
    let getQuery = C.unpack $ toString $ queryString [("api_secret", encodeUtf8 s), ("access_token", encodeUtf8 a), ("forum", encodeUtf8 f), ("limit", "6")]
    res <- submitPostRequest $ "https://disqus.com/api/3.0/threads/listPopular.json?" ++ getQuery
    case (eitherDecode res) of
        Left err -> return Nothing
        Right r -> return $ Just r
