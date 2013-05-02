module Main where

import Prelude hiding (catch)
import Control.Exception
import Test.HUnit
import Language.Haskell.TH
import Data.Default
import qualified Data.Map as M

import Web.Google.API.Discovery
import Web.Google.API.Discovery.TH

discardError :: IOError -> IO ()
discardError _ = return ()

testNoName = TestCase $
    let genValue = do
            runQ . generateType $ def
            assertFailure "Expected Name Required Error."
     in catch genValue discardError

assertSchemaGeneratesDec :: Dec -> JsonSchema -> IO ()
assertSchemaGeneratesDec expected schema = do
    generated <- runQ . generateType $ schema
    assertEqual "Generated and expected AST are not equal." expected generated

testMinimal = TestCase $ do
    let expected = DataD [] (mkName "FooBar") [] [] []
    testValue <- runQ . generateType $ def { jsonSchemaId = Just "FooBar" }
    assertEqual "testMinimal: " expected testValue

testStringProp = TestCase $ do
    let expected = DataD []
                         (mkName "Foo")
                         []
                         [RecC (mkName "Foo")
                         [(mkName "bar", NotStrict, AppT (ConT (mkName "Data.Maybe.Maybe")) (ConT (mkName "GHC.Base.String")))]]
                         []
        schema = def { jsonSchemaId = Just "Foo", jsonSchemaProperties = Just $ M.fromList [("bar", def { jsonSchemaType = Just "string" })]  }
    assertSchemaGeneratesDec expected schema

tests = TestList [TestLabel "Minimal" testMinimal, TestLabel "String Property" testStringProp]

main = runTestTT tests
