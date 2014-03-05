{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
module Handler.API.CloudFlare (
    getCloudFlareStats
  , CloudFlareAction(..)
  , CloudFlareAuth(..)
  , CloudFlareTrafficBreakdown(..)
  ) where

import Prelude hiding (head, init, last, readFile, tail, writeFile)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.ByteString
import Data.Conduit (MonadBaseControl)
import Data.Aeson
import Network.HTTP.Conduit
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)


-- | 'CloudFlareAuth' consists of a CloudFlare API key and the email tied to that key.
data CloudFlareAuth = CloudFlareAuth 
    { tkn   :: Text -- ^ This is the API key made available on your CloudFlare Account page.
    , email :: Text -- ^ The e-mail address associated with the API key.
    } deriving (Show)

-- | The CloudFlare action.
data CloudFlareAction = CloudFlareAction
    { z :: Text -- ^ The zone (domain) that the action is being performed on.
    , interval :: Text -- ^ The time interval for the statistics. 20, 30 and 40 is 30, 7 and 1 day respectively
    } deriving (Show, Generic)

-- | The traffic data from CloudFlare
data CloudFlareStats = CloudFlareStats 
    { regular :: Int
    , threat :: Int
    , crawler :: Int
    } deriving (Show, Generic)
    
instance ToJSON CloudFlareStats
instance FromJSON CloudFlareStats

-- | The traffic breakdown from CloudFlare
data CloudFlareTrafficBreakdown = CloudFlareTrafficBreakdown
    { pageviews :: CloudFlareStats
    , uniques :: CloudFlareStats
    } deriving (Show, Generic)

instance ToJSON CloudFlareTrafficBreakdown
instance FromJSON CloudFlareTrafficBreakdown where
    parseJSON (Object v) = CloudFlareTrafficBreakdown
        <$> ((v .: "trafficBreakdown") >>= (.: "pageviews"))
        <*> ((v .: "trafficBreakdown") >>= (.: "uniques"))
    parseJSON _ = mzero

-- | The response returned from the CloudFlare API.
data CloudFlareResponse = CloudFlareResponse 
    { objs :: [CloudFlareTrafficBreakdown]
    , result :: Text
    } deriving (Show, Generic)

instance FromJSON CloudFlareResponse where
    parseJSON (Object v) = CloudFlareResponse
        <$> (((v .: "response") >>= (.: "result")) >>= (.: "objs"))
        <*> v .: "result"
    parseJSON _ = mzero

{-|
  'submitPostRequest' sends the POST request to the url parameter, and return the response as a 'L.ByteString' wrapped in 'MonadIO' or 'MonadBaseControl IO' monad.
-}
submitPostRequest :: (MonadIO m, MonadBaseControl IO m) => String -> [(ByteString, ByteString)] -> m L.ByteString
submitPostRequest urlString postQuery = 
    case parseUrl urlString of
        Nothing -> return "URL Syntax Error"
        Just initReq -> withManager $ \manager -> do
            let req = initReq { secure = True
                              , requestHeaders = [("User-Agent", "HsCMS")]
                              , checkStatus = \_ _ _ -> Nothing
                              }
            let req2 = (flip urlEncodedBody) req $ postQuery
            res <- httpLbs req2 manager
            return $ responseBody res

{-|
  Sends a POST request to https://www.cloudflare.com/api_json.html with the relevant POST data, such as token, email, action, zone and interval, and returns a traffic breakdown of the domain (zone) requested.
-}
getCloudFlareStats :: (MonadIO m, MonadBaseControl IO m) => CloudFlareAuth -> CloudFlareAction -> m (Maybe CloudFlareTrafficBreakdown)
getCloudFlareStats (CloudFlareAuth t e) (CloudFlareAction zn i) = do
    let postQuery = [ ("tkn", encodeUtf8 t)
                    , ("email", encodeUtf8 e)
                    , ("a", "stats")
                    , ("z", encodeUtf8 zn)
                    , ("interval", encodeUtf8 i) 
                    ]
    res <- submitPostRequest "https://www.cloudflare.com/api_json.html" postQuery
    case eitherDecode res of
        Left _ -> return Nothing
        Right (CloudFlareResponse (traffic:_) _) -> return $ Just traffic
        Right (CloudFlareResponse [] _) -> return Nothing

