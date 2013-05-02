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
            name <- maybeName (jsonSchemaId schema)
            strict <- undefined
            typ <- undefined
            return (name, strict, typ)

