{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Google.YouTube ( listVideos
                          , module Web.Google.YouTube.Types
                          ) where

import Network.HTTP.Conduit ()

import qualified Data.ByteString.Char8 as BS
import Web.Google.YouTube.Types
import Web.Google.YouTube.VideoParser
import Web.Google.API.Rest as Rest


instance GoogleApiRequest VideoListRequest VideoListResponse where
    getPath = const "/youtube/v3/videos"
    getQuery vlr = [ ("id", Just . BS.intercalate "," . map BS.pack . videoId $ vlr)
                   , ("part", Just . BS.intercalate "," . map (BS.pack . show) . parts $ vlr)
                   ]
    getMethod = const methodGet

listVideos :: GoogleApiConfig -> [VideoId] -> [VideoInfoPart] -> IO (GoogleApiResult [Video])
listVideos api ids parts = do
    let req = VideoListRequest { videoId = ids, parts = parts }
    mResp <- runApiRequest api req
    return (vlr_items `fmap` mResp)

