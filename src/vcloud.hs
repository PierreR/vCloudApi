{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Main  where

import           Control.Lens
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as LB
import           Data.Monoid
import           Data.Text               (Text)
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import           Network.Wreq
import qualified Network.Wreq.Session    as S
import           Text.Xml.Lens

import           Option
import           Utils
import           VCloud.Namespace

vCloudURL = "https://c.irisnet.be/api"
vCloudVers = "1.5"
loginPath = vCloudURL <> "/login"

-- | we only query inside one vApp
vAppQueryPath id = "/vApp/vapp-" <> id <>"/ovf"

-- | Given a user, a pass and a vApp id, return the raw xml response lazily.
vCloudSession :: ByteString -> ByteString -> String -> IO LB.ByteString
vCloudSession user pass appid =
  S.withSessionControl (Just (HTTP.createCookieJar [])) ignoreTLSCertificatesSettings $ \sess -> do
    let opts = defaults & header "Accept" .~ ["application/*+xml;version=" <> vCloudVers]
        apiPath = vCloudURL <> vAppQueryPath appid
    _ <- S.getWith (opts & auth ?~ basicAuth user pass) sess loginPath
    r <- S.getWith opts sess apiPath
    return $ r ^. responseBody

fetchVM :: AsXmlDocument t => Text -> Traversal' t Element
fetchVM n = xml...ovfNode "VirtualSystem".attributed (ix (nsName ovfNS "id").only n)

fetchIP :: Traversal' Element Element
fetchIP = vcloudNode "NetworkConnectionSection"...vcloudNode "IpAddress"

main = do
  Options {..} <- cmdOpts
  raw <- vCloudSession vCloudUser vCloudPass vAppId
  print $ raw ^. fetchVM "jenkinsslave2".fetchIP.text
