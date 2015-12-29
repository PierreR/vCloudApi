{-# LANGUAGE RecordWildCards #-}
module Main  where

import           Control.Lens            (ix, only, (%~), (&), (...), (.~),
                                          (?~), (^.), (^..))
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
import Utils
import           VCloud.Namespace

vCloudURL = "https://c.irisnet.be/api"
vCloudVers = "1.5"
vAppQueryPath id = "/vApp/vapp-" <> id <>"/ovf"
loginPath = vCloudURL <> "/login"

vCloudSession :: ByteString -> ByteString -> String -> IO LB.ByteString
vCloudSession user pass appid =
  S.withSessionControl (Just (HTTP.createCookieJar [])) ignoreTLSCertificatesSettings $ \sess -> do
    let opts = defaults & header "Accept" .~ ["application/*+xml;version=" <> vCloudVers]
        apiPath = vCloudURL <> vAppQueryPath appid
    _ <- S.getWith (opts & auth ?~ basicAuth user pass) sess loginPath
    r <- S.getWith opts sess apiPath
    return $ r ^. responseBody


main = do
  Options {..} <- cmdOpts
  raw <- vCloudSession vCloudUser vCloudPass vAppId
  print $ raw ^. xml...ovfNode "VirtualSystem".attributed (ix (nsName ovfNS "id").only "jenkinsslave2").vcloudNode "NetworkConnectionSection"...vcloudNode "IpAddress".text
