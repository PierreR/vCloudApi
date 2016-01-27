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

import           Control.Lens
import           Data.Text     (Text)
import           Text.XML      (Prologue (Prologue), Document (Document), Name (Name))
import           Text.Xml.Lens

vcloudNS = "http://www.vmware.com/vcloud/v1.5"
ovfNS = "http://schemas.dmtf.org/ovf/envelope/1"

vcloudNode :: Text -> Traversal' Element Element
vcloudNode = nsNode vcloudNS

ovfNode :: Text -> Traversal' Element Element
ovfNode = nsNode ovfNS

nsNode :: Text -> Text -> Traversal' Element Element
nsNode ns n = node $ nsName ns n

nsName :: Text -> Text -> Name
nsName ns n = Name n (Just ns) Nothing
