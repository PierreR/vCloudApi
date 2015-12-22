{-# LANGUAGE RecordWildCards #-}
module Main  where

import           Control.Lens            ((%~), (&), (.~), (?~), (^.))
import           Data.ByteString         (ByteString)
import           Data.Monoid
import           Network.Connection      (TLSSettings (TLSSettingsSimple))
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import           Network.Wreq
import qualified Network.Wreq.Session    as S

import           Option

vCloudURL = "https://c.irisnet.be/api"
vCloudVers = "5.5"
loginPath = vCloudURL <> "/login"

ignoreTLSCertificatesSettings :: HTTP.ManagerSettings
ignoreTLSCertificatesSettings  = TLS.mkManagerSettings (TLSSettingsSimple True False False) Nothing

main :: IO ()
main = do
  Options {..} <- cmdOpts
  S.withSessionControl (Just (HTTP.createCookieJar [])) ignoreTLSCertificatesSettings $ \sess -> do
    let opts = defaults & header "Accept" .~ ["application/*+xml;version=" <> vCloudVers]
        apiPath = vCloudURL <> apiQuery
    _ <- S.getWith (opts & auth ?~ basicAuth vCloudUser vCloudPass) sess loginPath
    r <- S.getWith opts sess apiPath
    print $ r ^. responseBody
