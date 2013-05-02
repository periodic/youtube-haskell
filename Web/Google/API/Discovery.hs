{-# LANGUAGE OverloadedStrings #-}
module Web.Google.API.Discovery where

import Control.Applicative
import Data.Map (Map)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default

data Icons = Icons {
  directoryListItemIconsX16 :: Maybe String,
  directoryListItemIconsX32 :: Maybe String
} deriving (Show)

instance FromJSON Icons where
  parseJSON (Object v) = Icons
                        <$> optional (v .: "x16")
                        <*> optional (v .: "x32")

data JsonSchema = JsonSchema {
  jsonSchemaRef :: Maybe String,
  jsonSchemaAdditionalProperties :: Maybe JsonSchema,
  jsonSchemaAnnotations :: Maybe JsonSchemaAnnotations,
  jsonSchemaDefault :: Maybe String,
  jsonSchemaDescription :: Maybe String,
  jsonSchemaEnum :: Maybe [String],
  jsonSchemaEnumDescriptions :: Maybe [String],
  jsonSchemaFormat :: Maybe String,
  jsonSchemaId :: Maybe String,
  jsonSchemaItems :: Maybe JsonSchema,
  jsonSchemaLocation :: Maybe String,
  jsonSchemaMaximum :: Maybe String,
  jsonSchemaMinimum :: Maybe String,
  jsonSchemaPattern :: Maybe String,
  jsonSchemaProperties :: Maybe (Map String JsonSchema),
  jsonSchemaReadOnly :: Maybe Bool,
  jsonSchemaRepeated :: Maybe Bool,
  jsonSchemaRequired :: Maybe Bool,
  jsonSchemaType :: Maybe String
} deriving (Show)

instance Default JsonSchema where
    def = JsonSchema {
        jsonSchemaRef = Nothing,
        jsonSchemaAdditionalProperties = Nothing,
        jsonSchemaAnnotations = Nothing,
        jsonSchemaDefault = Nothing,
        jsonSchemaDescription = Nothing,
        jsonSchemaEnum = Nothing,
        jsonSchemaEnumDescriptions = Nothing,
        jsonSchemaFormat = Nothing,
        jsonSchemaId = Nothing,
        jsonSchemaItems = Nothing,
        jsonSchemaLocation = Nothing,
        jsonSchemaMaximum = Nothing,
        jsonSchemaMinimum = Nothing,
        jsonSchemaPattern = Nothing,
        jsonSchemaProperties = Nothing,
        jsonSchemaReadOnly = Nothing,
        jsonSchemaRepeated = Nothing,
        jsonSchemaRequired = Nothing,
        jsonSchemaType = Nothing
    }

instance FromJSON JsonSchema where
  parseJSON (Object v) = JsonSchema
                        <$> optional (v .: "$ref")
                        <*> optional (v .: "additionalProperties")
                        <*> optional (v .: "annotations")
                        <*> optional (v .: "default")
                        <*> optional (v .: "description")
                        <*> optional (v .: "enum")
                        <*> optional (v .: "enumDescriptions")
                        <*> optional (v .: "format")
                        <*> optional (v .: "id")
                        <*> optional (v .: "items")
                        <*> optional (v .: "location")
                        <*> optional (v .: "maximum")
                        <*> optional (v .: "minimum")
                        <*> optional (v .: "pattern")
                        <*> optional (v .: "properties")
                        <*> optional (v .: "readOnly")
                        <*> optional (v .: "repeated")
                        <*> optional (v .: "required")
                        <*> optional (v .: "type")

data JsonSchemaAnnotations = JsonSchemaAnnotations {
  jsonSchemaAnnotationsRequired :: Maybe [String]
} deriving (Show)

instance FromJSON JsonSchemaAnnotations where
  parseJSON (Object v) = JsonSchemaAnnotations
                        <$> optional (v .: "required")


data RestDescription = RestDescription {
  restDescriptionAuth :: Maybe RestDescriptionAuth,
  restDescriptionBasePath :: Maybe String,
  restDescriptionBaseUrl :: Maybe String,
  restDescriptionBatchPath :: Maybe String,
  restDescriptionCanonicalName :: Maybe String,
  restDescriptionDescription :: Maybe String,
  restDescriptionDiscoveryVersion :: Maybe String,
  restDescriptionDocumentationLink :: Maybe String,
  restDescriptionEtag :: Maybe String,
  restDescriptionFeatures :: Maybe [String],
  restDescriptionIcons :: Maybe Icons,
  restDescriptionId :: Maybe String,
  restDescriptionKing :: Maybe String,
  restDescriptionLabels :: Maybe [String],
  restDescriptionMethods :: Maybe (Map String RestMethod),
  restDescriptionName :: Maybe String,
  restDescriptionParameters :: Maybe (Map String JsonSchema),
  restDescriptionProtocol :: Maybe String,
  restDescriptionResources :: Maybe (Map String RestResource),
  restDescriptionRevision :: Maybe String,
  restDescriptionRootUrl :: Maybe String,
  restDescriptionSchemas :: Maybe (Map String JsonSchema),
  restDescriptionServicePath :: Maybe String,
  restDescriptionTitle :: Maybe String,
  restDescriptionVersion :: Maybe String
} deriving (Show)

instance FromJSON RestDescription where
  parseJSON (Object v) = RestDescription
                        <$> optional (v .: "auth")
                        <*> optional (v .: "basePath")
                        <*> optional (v .: "baseUrl")
                        <*> optional (v .: "batchPath")
                        <*> optional (v .: "canonicalName")
                        <*> optional (v .: "description")
                        <*> optional (v .: "discoveryVersion")
                        <*> optional (v .: "documentationLink")
                        <*> optional (v .: "etag")
                        <*> optional (v .: "features")
                        <*> optional (v .: "icons")
                        <*> optional (v .: "id")
                        <*> optional (v .: "king")
                        <*> optional (v .: "labels")
                        <*> optional (v .: "methods")
                        <*> optional (v .: "name")
                        <*> optional (v .: "parameters")
                        <*> optional (v .: "protocol")
                        <*> optional (v .: "resources")
                        <*> optional (v .: "revision")
                        <*> optional (v .: "rootUrl")
                        <*> optional (v .: "schemas")
                        <*> optional (v .: "servicePath")
                        <*> optional (v .: "title")
                        <*> optional (v .: "version")

data RestDescriptionAuth = RestDescriptionAuth {
} deriving (Show)

instance FromJSON RestDescriptionAuth where
  parseJSON _ = return RestDescriptionAuth

data RestMethod = RestMethod {
} deriving (Show)

instance FromJSON RestMethod where
  parseJSON _ = return RestMethod

data RestResource = RestResource {
} deriving (Show)

instance FromJSON RestResource where
  parseJSON _ = return RestResource
