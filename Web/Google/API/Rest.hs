{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Google.API.Rest (simpleRequest, httpRequest, GoogleApiRequest(..), methodGet, methodPost, GoogleApiConfig, apiKey, def) where

import Control.Applicative
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as Json
import qualified Data.Attoparsec as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Map (Map)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Data.Default

data GoogleApiConfig = GApiConf {
    apiKey :: BS.ByteString
    } deriving (Show, Eq, Read, Ord)

instance Default GoogleApiConfig where
    def = GApiConf ""

class (Json.FromJSON b) => GoogleApiRequest a b | a -> b where
    requiresAuth :: a -> Bool
    requiresAuth _ = False
    getPath :: a -> String
    getQuery :: a -> Query
    getMethod :: a -> Method

buildHttpRequest :: (GoogleApiRequest a b) => GoogleApiConfig -> a -> Request m
buildHttpRequest api req = Network.HTTP.Conduit.def { method = getMethod req
                               , host = "www.googleapis.com"
                               , port = 443
                               , secure = True
                               , path = C8.pack . getPath $ req
                               , queryString = renderQuery False . (("key", Just $ apiKey api):) . getQuery $ req
                               }

simpleRequest :: (Json.FromJSON b, GoogleApiRequest a b) => GoogleApiConfig -> a -> IO (Maybe b)
simpleRequest api req = do
    let request = buildHttpRequest api req
    res <- withManager $ httpLbs request
    return . Json.decode . responseBody $ res

httpRequest :: (MonadResource m, MonadBaseControl IO m, GoogleApiRequest a b) => GoogleApiConfig -> a -> Manager -> m (Maybe b)
httpRequest api req manager = do
    let request = buildHttpRequest api req
    bodySource <- responseBody <$> http request manager
    parsedBody <- bodySource $$+- sinkParser (Json.fromJSON <$> Json.json)
    case parsedBody of
        Json.Success resp -> return $ Just resp
        _                 -> return Nothing
