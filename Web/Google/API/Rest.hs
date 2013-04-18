{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Google.API.Rest ( GoogleApiRequest(..)
                           , GoogleApiConfig(..)
                           , GoogleApiAuthConfig(..)
                           , GoogleApiResult(..)
                           , def
                           , methodGet
                           , runApiRequest) where

import Control.Applicative
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as Json
import qualified Data.Attoparsec as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Map (Map)
import Network.HTTP.Conduit hiding (def)
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Types
import Data.Default

data GoogleApiAuthConfig = GoogleApiUnauthenticated
                         | GoogleApiKey BS.ByteString
                         | GoogleApiOAuth
                         deriving (Show, Eq)

data GoogleApiResult a = Success a
                       | Failure BS.ByteString
                       deriving (Show)

instance Functor GoogleApiResult where
    fmap f (Success a)   = Success (f a)
    fmap _ (Failure msg) = Failure msg

data GoogleApiConfig = GApiConf
    { getApiAuth :: GoogleApiAuthConfig
    , getApiEndpoint :: BS.ByteString
    } deriving (Show, Eq)

instance Default GoogleApiConfig where
    def = GApiConf GoogleApiUnauthenticated "www.googleapis.com"

runApiRequest :: (GoogleApiRequest a b) => GoogleApiConfig -> a -> IO (GoogleApiResult b)
runApiRequest config req =
    withManager $ \manager -> do
        let request = buildHttpRequest config req
        bodySource <- responseBody <$> http request manager
        parsedBody <- bodySource $$+- sinkParser (Json.fromJSON <$> Json.json)
        case parsedBody of
            Json.Success resp -> return $ Success resp
            _                 -> return $ Failure ""

class (Json.FromJSON b) => GoogleApiRequest a b | a -> b where
    getPath :: a -> BS.ByteString
    getQuery :: a -> Query
    getMethod :: a -> Method

{-
class GoogleApi m where
    request :: (MonadResource m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m, GoogleApiRequest a b) => a -> m (Maybe b)
    getConfig :: m GoogleApiConfig

getFromConfig :: (GoogleApi m, Functor m) => (GoogleApiConfig -> a) -> m a
getFromConfig = (<$> getConfig)

newtype GoogleApiT m a = GoogleApiT { runGoogleApiT :: GoogleApiConfig -> m a }

instance Monad m => GoogleApi (GoogleApiT m) where
    getConfig = GoogleApiT (\conf -> return conf)
    request req = withManager $ \manager -> do
        conf <- getConfig
        let request = buildHttpRequest conf req
        bodySource <- responseBody <$> http request manager
        parsedBody <- bodySource $$+- sinkParser (Json.fromJSON <$> Json.json)
        case parsedBody of
            Json.Success resp -> return $ Just resp
            _                 -> return Nothing


instance Monad m => Monad (GoogleApiT m) where
    return a = GoogleApiT $ \conf -> return a
    x >>= f  = GoogleApiT $ \conf -> do
        x_val <- runGoogleApiT x conf
        runGoogleApiT (f x_val) conf
-}

buildHttpRequest :: (GoogleApiRequest a b) => GoogleApiConfig -> a -> Request m
buildHttpRequest api req 
    = HTTP.def { method = getMethod req
               , host = getApiEndpoint api
               , port = 443
               , secure = True
               , path = getPath req
               , queryString = renderQuery False query
               }
    where
        query = case getApiAuth api of
                 GoogleApiUnauthenticated -> getQuery req
                 GoogleApiKey key         -> ("key", Just key) : getQuery req
                 GoogleApiOAuth           -> undefined

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
