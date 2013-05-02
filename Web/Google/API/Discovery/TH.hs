{-# LANGUAGE TemplateHaskell #-}
module Web.Google.API.Discovery.TH where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Haskell.TH
import Data.Default

import Web.Google.API.Discovery


generateTypes :: Map String JsonSchema -> Q [Dec]
generateTypes schemaMap = mapM generateType . M.elems $ schemaMap

generateType :: JsonSchema -> Q Dec
generateType schema = do
    name <- maybeName (jsonSchemaId schema)
    fields <- generateFields $ jsonSchemaProperties schema
    let con = RecC name fields
    return $ DataD [] name [] [] []
    where
        maybeName = maybe (fail "Name Required") (return . mkName)
        generateFields mProps = case mProps of
            Nothing -> return []
            Just props -> mapM generateField $ M.toList props
        generateField (nameStr,schema) = do
            let name = mkName nameStr
            typ <- getType False $ jsonSchemaType schema
            return (name, NotStrict, typ)

getType :: Bool -> Maybe String -> Q Type
getType _ Nothing = fail "Name Required"
getType True (Just name) = case name of
    "string" -> return $ ConT (mkName "GHC.Base.String")
    _        -> fail "Unsupported type."
getType False typ = do
    inner <- getType False typ
    return $ AppT (ConT (mkName "Data.Maybe.Maybe")) inner
