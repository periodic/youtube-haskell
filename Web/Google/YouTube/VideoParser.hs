{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Web.Google.YouTube.VideoParser where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable as T
import Web.Google.API.Rest

import Web.Google.YouTube.Types


instance FromJSON Video where
    parseJSON (Object v) = Video 
                        <$> v .: "id"
                        <*> v .: "kind"
                        <*> v .: "etag"
                        <*> optional (v .: "snippet")
                        <*> optional (v .: "contentDetails")
                        <*> optional (v .: "player")
                        <*> optional (v .: "statistics")
                        <*> optional (v .: "status")
                        <*> optional (v .: "topicDetails" >>= (.: "topicIds"))
    parseJSON _ = mzero

instance FromJSON Snippet where
    parseJSON (Object v) = Snippet
                        <$> v .: "publishedAt"
                        <*> v .: "channelId"
                        <*> v .: "title"
                        <*> v .: "description"
                        <*> (v .: "thumbnails" >>= flattenThumbnails)
                        <*> v .: "categoryId"
                        <*> optional (v .: "tags")
        where
            flattenThumbnails :: Map String Object -> Data.Aeson.Types.Parser (Map String String)
            flattenThumbnails = T.mapM (.: "url")
    parseJSON _ = mzero

instance FromJSON ContentDetails where
    parseJSON (Object v) = ContentDetails
                        <$> v .: "duration"
                        <*> optional (v .: "regionRestriction")
    parseJSON _ = mzero

instance FromJSON RegionRestriction where
    parseJSON (Object v) = RegionRestriction
                        <$> v .: "allowed"
                        <*> v .: "blocked"
    parseJSON _ = mzero

instance FromJSON Player where
    parseJSON (Object v) = Player
                        <$> v .: "embedHtml"
    parseJSON _ = mzero

instance FromJSON Statistics where
    parseJSON (Object v) = Statistics
                        <$> v .: "viewCount"
                        <*> v .: "likeCount"
                        <*> v .: "dislikeCount"
                        <*> v .: "favoriteCount"
                        <*> v .: "commentCount"
    parseJSON _ = mzero

instance FromJSON Status where
    parseJSON (Object v) = Status
                        <$> v .: "uploadStatus"
                        <*> optional (v .: "failureReason")
                        <*> optional (v .: "rejectedReason")
                        <*> v .: "privacyStatus"
    parseJSON _ = mzero

instance FromJSON VideoListResponse where
    parseJSON (Object v) = VideoListResponse
                        <$> v .: "kind"
                        <*> v .: "etag"
                        <*> v .: "items"
    parseJSON _ = mzero

instance GoogleApiRequest VideoListRequest VideoListResponse where
    getPath = const "/youtube/v3/videos"
    getQuery vlr = [ ("id", Just . videoId $ vlr)
                   , ("key", Just . key $ vlr)
                   , ("part", Just . BS.intercalate "," . map (BS.pack . show) . parts $ vlr)
                   ]
    getMethod = const methodGet
