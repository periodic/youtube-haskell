{-# LANGUAGE OverloadedStrings  #-}
module Web.Google.YouTube.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Network.HTTP.Types (methodGet)
import Web.Google.API.Rest

type APIKey = ByteString
type VideoId = String

data VideoInfoPart = SnippetPart
                   | ContentDetailsPart
                   | PlayerPart
                   | StatisticsPart
                   | StatusPart
                   | TopicDetailsPart

instance Show VideoInfoPart where
    show ContentDetailsPart = "contentDetails"
    show PlayerPart = "player"
    show SnippetPart = "snippet"
    show StatisticsPart = "statistics"
    show StatusPart = "status"
    show TopicDetailsPart = "topicDetails"

data Video = Video { video_id :: String -- appears required
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

data Statistics = Statistics { viewCount :: String -- These are strings... how to parse them to integers inside the aeson Monad...
                             , likeCount :: String
                             , dislikeCount :: String
                             , favoriteCount :: String
                             , commentCount :: String
                             } deriving (Show)

data Status = Status { uploadStatus :: String -- Appears required
                     , failureReason :: Maybe String
                     , rejectedReason :: Maybe String
                     , privacyStatus :: String -- Appears required
                     } deriving (Show)

-- https://www.googleapis.com/youtube/v3/videos?id=9bZkp7q19f0&key=AIzaSyC6mnIp_OtG4TEVIdA2WzQViAcU4lAIa4Q&part='$part
data VideoListRequest = VideoListRequest { videoId :: [VideoId]
                                         , parts :: [VideoInfoPart]
                                         } deriving (Show)
data VideoListResponse = VideoListResponse { vlr_kind :: String
                                           , vlr_etag :: String
                                           , vlr_items :: [Video]
                                           } deriving (Show)


