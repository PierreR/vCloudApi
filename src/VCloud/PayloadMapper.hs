{-# LANGUAGE QuasiQuotes #-}
-- | Map action to request payload
module VCloud.PayloadMapper where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Map             as Map
import qualified Text.Hamlet.XML      as H
import qualified Text.XML             as XML

import           Utils
import           VCloud.Namespace
import           VCloud.Prelude

createSnapshotXml :: Document
createSnapshotXml = createXml $
   Element "CreateSnapshotParams" (Map.fromList [("xmlns", vcloudNS), ("name", "cicd-vcloudapi")])
           [H.xml|
             <Description>
           |]

payloads :: Map String (Maybe ByteString, LB.ByteString)
payloads = Map.fromList [ ("revertToCurrentSnapshot", (Nothing, LB.empty))
                        , ("createSnapshot", (Just "application/vnd.vmware.vcloud.createSnapshotParams+xml", XML.renderLBS XML.def createSnapshotXml))
                        ]

lookupPayload ::  String -> IO (Maybe ByteString, LazyByteString)
lookupPayload key =
  case Map.lookup key payloads of
    Nothing -> putStrLn ("Unknown action " <> key) *> exitFailure
    Just x  -> return x
