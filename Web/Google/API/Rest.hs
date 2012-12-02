{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Google.API.Rest (simpleRequest, httpRequest, GoogleApiRequest(..), methodGet, methodPost) where

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

class (Json.FromJSON b) => GoogleApiRequest a b | a -> b where
    getPath :: a -> String
    getQuery :: a -> Query
    getMethod :: a -> Method

buildHttpRequest :: GoogleApiRequest a b => a -> Request m
buildHttpRequest req = def { method = getMethod req
                      , host = "www.googleapis.com"
                      , port = 443
                      , secure = True
                      , path = C8.pack . getPath $ req
                      , queryString = renderQuery False . getQuery $ req
                      }

simpleRequest :: (Json.FromJSON b, GoogleApiRequest a b) => a -> IO (Maybe b)
simpleRequest req = do
    let request = buildHttpRequest req
    res <- withManager $ httpLbs request
    return . Json.decode . responseBody $ res

-- (MonadResource m, MonadBaseControl IO m)
httpRequest :: (MonadResource m, MonadBaseControl IO m, GoogleApiRequest a b) => a -> Manager -> m (Maybe b)
httpRequest req manager = do
    let request = buildHttpRequest req
    bodySource <- responseBody <$> http request manager
    parsedBody <- bodySource $$+- sinkParser (Json.fromJSON <$> Json.json)
    case parsedBody of
        Json.Success resp -> return $ Just resp
        _                 -> return Nothing
