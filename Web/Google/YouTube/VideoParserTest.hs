module Main where

import Web.Google.YouTube.Types
import Web.Google.YouTube.VideoParser
import Text.Printf

import qualified Data.Attoparsec as P
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Control.Applicative

import System.Exit (exitFailure, exitSuccess)

import Data.Maybe

parseFile :: String -> IO (P.Result (Json.Result VideoListResponse))
parseFile path = BS.readFile path >>= return . P.parse (Json.fromJSON <$> Json.json)

expect :: FilePath -> (VideoListResponse -> Bool) -> IO Bool
expect path test = do
    res <- parseFile path
    case res of
        P.Done _ (Json.Success listResp) -> return $ test listResp
        _                                -> return False

hasSection :: (Video -> Maybe a) -> VideoListResponse -> Bool
hasSection section resp = let items     = vlr_items resp
                              hasLength = length items > 0
                              runTest   = isJust . section . head $ items
                           in hasLength && runTest

noSection :: (Video -> Maybe a) -> VideoListResponse -> Bool
noSection section = not . hasSection section

main = do
    testResults <- mapM runTest [ ("testdata/empty.json", noSection snippet)
                                , ("testdata/snippet.json", hasSection snippet)
                                , ("testdata/contentDetails.json", hasSection contentDetails)
                                , ("testdata/player.json", hasSection player)
                                , ("testdata/statistics.json", hasSection statistics)
                                , ("testdata/status.json", hasSection status)
                                , ("testdata/topicDetails.json", hasSection topicDetails)
                                ]
    if and testResults
        then exitSuccess
        else exitFailure
    where
        runTest (path, test) = do
            result <- expect path test
            printf "%s: %s\n" path (show result)
            return result

