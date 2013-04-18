{-# LANGUAGE OverloadedStrings #-}
module Web.Google.YouTubeTest where

import Control.Applicative

import System.Exit (exitFailure, exitSuccess)

import Web.Google.API.Rest
import Web.Google.YouTube

fetchVideoTest :: IO Bool
fetchVideoTest = do
    let apiConfig = def { getApiAuth = GoogleApiKey "AIzaSyC6mnIp_OtG4TEVIdA2WzQViAcU4lAIa4Q" }
        gangnamId = "9bZkp7q19f0"
        videoListRequest = VideoListRequest { 
            videoId = [gangnamId],
            parts = [SnippetPart]
        }
    apiResult <- runApiRequest apiConfig videoListRequest
    case apiResult of
        Success vlr -> return . (== [gangnamId]) . (video_id <$>) . vlr_items $ vlr 
        _           -> return False

main = do
    testResults <- sequence [fetchVideoTest]
    if and testResults
        then exitSuccess
        else exitFailure
