{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Flag where

import Conduit qualified as C
import Conduit qualified as HC
import Data.Generics
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Network.Connection
import Network.HTTP.Conduit qualified as HC
import Text.HTML.DOM qualified as D
import Text.XML

extensionHtmlUrl :: String
extensionHtmlUrl = "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/table.html"

downloadExtensionHtml :: IO Document
downloadExtensionHtml = do
  request <- HC.parseRequest extensionHtmlUrl
  manager <- HC.newManager $ HC.mkManagerSettings (TLSSettingsSimple True False False def) Nothing
  C.runResourceT $ do
    response <- HC.http request manager
    HC.runConduit $ HC.responseBody response C..| D.sinkDoc

-- TODO: extract `tr` instead of `table`
extractExtensionTable :: Document -> Maybe Element
extractExtensionTable doc = listToMaybe $ everything (++) ([] `mkQ` findTable) (documentRoot doc)
  where
    findTable :: Element -> [Element]
    findTable el
      | el.elementName == "table" && hasClass "longtable docutils align-default" el = [el]
      | otherwise = []

    hasClass :: T.Text -> Element -> Bool
    hasClass cls el = case Map.lookup "class" (elementAttributes el) of
      Just val -> cls `T.isInfixOf` val
      Nothing -> False

-- _test = do
--   doc <- downloadExtensionHtml
--   let tb = extractExtensionTable doc
--   print tb
