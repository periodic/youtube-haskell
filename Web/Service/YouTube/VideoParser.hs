{-# LANGUAGE OverloadedStrings #-}
module Web.Service.YouTube.VideoParser where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import qualified Data.ByteString as BS
import Data.Traversable as T

import Web.Service.YouTube.Types


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
                        <*> optional (v .: "topicDetails")
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

parseFile path = BS.readFile path >>= return . parse (fromJSON <$> json)

parseData input = parse (fromJSON <$> json) input
