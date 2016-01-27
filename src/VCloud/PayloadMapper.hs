{-# LANGUAGE QuasiQuotes     #-}
-- | Map action to request payload
module VCloud.PayloadMapper where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString.Lazy    as LB
import           Text.XML
import qualified Text.Hamlet.XML as H
import System.Exit (exitFailure)
import           Data.Monoid

import           Utils
import           VCloud.Namespace

createSnapshotXml :: Document
createSnapshotXml = createXml $
   Element "CreateSnapshotParams" (Map.fromList [("xmlns", vcloudNS), ("name", "cicd-vcloudapi")])
           [H.xml|
             <description>
           |]

payloads :: Map String LB.ByteString
payloads = Map.fromList [ ("revertToCurrentSnapshot", LB.empty)
                        , ("createSnapshot", renderLBS def createSnapshotXml)
                        ]

lookupPayload ::  String -> IO LB.ByteString
lookupPayload key =
  case Map.lookup key payloads of
    Nothing -> putStrLn ("Unknown action " <> key) *> exitFailure
    Just x  -> return x
