{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.API where
 
import Core.Import
import Handler.API.CloudFlare
import Handler.API.Disqus
 
 
data CloudFlareResponseJson = CloudFlareResponseJson 
    Bool 
    (Either Text CloudFlareTrafficBreakdown) 
    deriving (Show)

instance ToJSON CloudFlareResponseJson where
     toJSON (CloudFlareResponseJson status r) = object ["success" .= status, "result" .= r]

data DisqusResponseJson = DisqusResponseJson 
    Bool 
    (Either Text DisqusResponse) 
    deriving (Show)

instance ToJSON DisqusResponseJson where
     toJSON (DisqusResponseJson status r) = object ["success" .= status, "result" .= r]

getAdminCloudFlareStatsR :: Handler Value
getAdminCloudFlareStatsR = do
    extra <- getExtra
    stats <- case extraCloudflareKey extra of
        Nothing -> return Nothing
        Just cfKey -> 
            case extraCloudflareMail extra of
            Nothing -> return Nothing
            Just cfMail -> 
                case extraCloudflareZone extra of
                    Nothing -> return Nothing
                    Just cfZone -> return =<< getCloudFlareStats (cloudFlareAuth cfKey cfMail) $ cloudFlareAction cfZone "20"
    return $ maybe errJson succJson stats
    where
        errJson = toJSON $ CloudFlareResponseJson False (Left "Error parsing CloudFlare JSON")
        succJson traffic = toJSON $ CloudFlareResponseJson True (Right traffic)

getAdminDisqusStatsR :: Handler Value
getAdminDisqusStatsR = do
    extra <- getExtra
    stats <- case extraDisqusSecretKey extra of
        Nothing -> return Nothing
        Just disSecret -> 
            case extraDisqusAccessToken extra of
            Nothing -> return Nothing
            Just disToken -> 
                case extraDisqus extra of 
                    Nothing -> return Nothing
                    Just dis -> return =<< getDisqusStats $ disqusRequest disSecret disToken dis
    return $ maybe errJson succJson stats
    where
        errJson = toJSON $ DisqusResponseJson False (Left "Error parsing Disqus JSON")
        succJson s = toJSON $ DisqusResponseJson True (Right s)

