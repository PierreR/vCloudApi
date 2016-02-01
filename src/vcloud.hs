{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Main  where

import           Control.Concurrent      (threadDelay)
import           Control.Lens
import qualified Data.Map as Map
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as LB
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import           Network.Wreq
import qualified Network.Wreq.Session    as S
import           Text.Xml.Lens

import           Option
import           Utils
import           VCloud.Namespace
import           VCloud.PayloadMapper

vCloudVers = "1.5"

fetchVM :: AsXmlDocument t => Text -> Traversal' t Element
fetchVM n = xml...ovfNode "VirtualSystem".attributed (ix (nsName ovfNS "id").only n)

fetchVmId :: Traversal' Element Element
fetchVmId = vcloudNode "GuestCustomizationSection" .vcloudNode "VirtualMachineId"

fetchIP :: Traversal' Element Element
fetchIP = vcloudNode "NetworkConnectionSection"...vcloudNode "IpAddress"

main = do
  Options {..} <- cmdOpts
  let apiUrl = vCloudURL <> "/api"
      loginPath = apiUrl <> "/login"
      -- we only query inside one vApp
      vAppQueryPath = apiUrl <> "/vApp/vapp-" <> vAppId <>"/ovf"
      -- query a specific virtual machine
  S.withSessionControl (Just (HTTP.createCookieJar [])) ignoreTLSCertificatesSettings $ \sess -> do
    let opts = defaults & header "Accept" .~ ["application/*+xml;version=" <> vCloudVers]
    _ <- S.getWith (opts & auth ?~ basicAuth vCloudUser vCloudPass) sess loginPath
    raw0 <- S.getWith opts sess vAppQueryPath
    let vmId = raw0 ^. responseBody . fetchVM vmName . fetchVmId.text
        actionUri = apiUrl <> "/vApp/vm-" <> Text.unpack vmId <> "/action/" <> vmAction
    (contentType, payload) <- lookupPayload vmAction
    let postOpts = maybe opts (\x -> opts & header "Content-Type" .~ [x]) contentType
    raw1 <- S.postWith postOpts sess actionUri payload
    print $ raw1 ^. responseStatus.statusMessage
    let taskLink = raw1 ^. responseHeader "Location"
        actionStatus n  = putStrLn "Waiting for status"
                          *> threadDelay (n*1000*1000)
                          *> do
                             r <- S.getWith opts sess (BC.unpack taskLink)
                             case r ^. responseBody .xml.attr "status" of
                               Just "success" -> putStrLn "success"
                               Just "running" -> actionStatus 10
                               _              -> putStrLn "fails"
    actionStatus 10
