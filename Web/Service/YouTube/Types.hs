{-# LANGUAGE OverloadedStrings #-}
module Web.Service.YouTube.Types where

import Data.Default
import Data.ByteString.Lazy as BS
import Data.Map (Map)
import qualified Data.Map as M
data YouTube = YouTube {
    apiKey :: ByteString
    } deriving (Show, Eq, Read, Ord)

instance Default YouTube where
    def = YouTube ""

data VideoId = String

data VideoInfoPart = SnippetPart
                   | ContentDetailsPart
                   | PlayerPart
                   | StatisticsPart
                   | StatusPart
                   | TopicDetailsPart
                   deriving (Show)

data Video = Video { id :: String -- appears required
                   , kind :: String -- appears required
                   , etag :: String -- appears required
                   , snippet :: Maybe Snippet
                   , contentDetails :: Maybe ContentDetails
                   , player :: Maybe Player
                   , statistics :: Maybe Statistics
                   , status :: Maybe Status
                   , topicDetails :: Maybe [TopicId]
                   } deriving (Show)

type TopicId = String
type Thumbnails = Map String String

data Snippet = Snippet { publishedAt :: String
                       , channelId :: String
                       , title :: String
                       , description :: String
                       , thumbnails :: Thumbnails
                       , categoryId :: String
                       , tags :: Maybe [String]
                       } deriving (Show)

data ContentDetails = ContentDetails { duration :: String
                                     , regionRestriction :: Maybe RegionRestriction
                                     } deriving (Show)

data RegionRestriction = RegionRestriction { allowed :: [String]
                                           , blocked :: [String]
                                           } deriving (Show)

data Player = Player { embedHtml :: String } deriving (Show)

data Statistics = Statistics { viewCount :: Integer
                             , likeCount :: Integer
                             , dislikeCount :: Integer
                             , favoriteCount :: Integer
                             , commentCount :: Integer
                             } deriving (Show)

data Status = Status { uploadStatus :: String -- Appears required
                     , failureReason :: Maybe String
                     , rejectedReason :: Maybe String
                     , privacyStatus :: String -- Appears required
                     } deriving (Show)

data VideoListResponse = VideoListResponse { vlr_kind :: String
                                           , vlr_etag :: String
                                           , vlr_items :: [Video]
                                           } deriving (Show)

