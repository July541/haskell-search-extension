{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Error where

import Conduit qualified as C
import Conduit qualified as HC
import Data.Aeson
import Data.ByteString.Lazy.UTF8 qualified as LBS
import Data.Default.Class (def)
import Data.List (intercalate)
import GHC.Generics
import Network.Connection
import Network.HTTP.Conduit qualified as HC

data Metadata = Metadata
  { introduced :: !String
  , severity :: !(Maybe String) -- Use `Maybe` because https://github.com/haskellfoundation/error-message-index/issues/538
  , summary :: !String
  , title :: !String
  }
  deriving (Eq, Show, Generic, FromJSON)

data Error = Error
  { code :: !String
  , metadata :: !Metadata
  , route :: !String
  }
  deriving (Eq, Show, Generic, FromJSON)

errorJsonUrl :: String
errorJsonUrl = "https://errors.haskell.org/api/errors.json"

downloadErrorJson :: IO LBS.ByteString
downloadErrorJson = do
  request <- HC.parseRequest errorJsonUrl
  manager <- HC.newManager $ HC.mkManagerSettings (TLSSettingsSimple True False False def) Nothing
  C.runResourceT $ do
    response <- HC.http request manager
    HC.runConduit $ HC.responseBody response C..| C.sinkLazy

parseErrors :: LBS.ByteString -> [Error]
parseErrors json = case decode json of
  Just errs -> errs
  Nothing -> error "Failed to parse errors"

generateTSFileForErrors :: FilePath -> IO ()
generateTSFileForErrors file = do
  json <- downloadErrorJson
  let errors = parseErrors json
  writeFile file $
    "export const errorRawData = "
      <> toCommaListString
        ( map
            ( \e ->
                toCommaListString
                  [ show e.code
                  , show e.metadata.title
                  , show e.route
                  , show e.metadata.introduced
                  ]
            )
            errors
        )
      <> ";"
  where
    toCommaListString :: [String] -> String
    toCommaListString ls = "[" <> intercalate "," ls <> "]"
