{-# LANGUAGE OverloadedStrings #-}
module Web.Service.YouTube ( YouTube
                           , apiKey
                           ) where

import Network.HTTP.Conduit ()

import Web.Service.YouTube.Types

listVideos :: [VideoId] -> [VideoInfoPart] -> Video
listVideos = undefined

