{-# LANGUAGE RankNTypes #-}
module VCloud.Namespace
(
    nsName
  , ovfNS
  , ovfNode
  , vcloudNode
  , vcloudNS
)
where

import qualified Text.XML       as XML
import qualified Text.Xml.Lens  as XML

import           VCloud.Prelude

vcloudNS = "http://www.vmware.com/vcloud/v1.5"
ovfNS = "http://schemas.dmtf.org/ovf/envelope/1"

vcloudNode :: Text -> Traversal' Element Element
vcloudNode = nsNode vcloudNS

ovfNode :: Text -> Traversal' Element Element
ovfNode = nsNode ovfNS

nsNode :: Text -> Text -> Traversal' Element Element
nsNode ns n = XML.node $ nsName ns n

nsName :: Text -> Text -> XML.Name
nsName ns n = XML.Name n (Just ns) Nothing
