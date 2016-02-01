{-# LANGUAGE QuasiQuotes #-}
-- | Map action to request payload
module VCloud.PayloadMapper where

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Monoid
import           System.Exit          (exitFailure)
import qualified Text.Hamlet.XML      as H
import           Text.XML

import           Utils
import           VCloud.Namespace

createSnapshotXml :: Document
createSnapshotXml = createXml $
   Element "CreateSnapshotParams" (Map.fromList [("xmlns", vcloudNS), ("name", "cicd-vcloudapi")])
           [H.xml|
             <description>
           |]

payloads :: Map String (Maybe ByteString, LB.ByteString)
payloads = Map.fromList [ ("revertToCurrentSnapshot", (Nothing, LB.empty))
                        , ("createSnapshot", (Just "application/vnd.vmware.vcloud.createSnapshotParams+xml", renderLBS def createSnapshotXml))
                        ]

lookupPayload ::  String -> IO (Maybe ByteString, LB.ByteString)
lookupPayload key =
  case Map.lookup key payloads of
    Nothing -> putStrLn ("Unknown action " <> key) *> exitFailure
    Just x  -> return x
