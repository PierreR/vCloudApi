module Utils where

import qualified Data.Text.Lazy.IO       as Text
import           Network.Connection      (TLSSettings (TLSSettingsSimple))
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as TLS

ignoreTLSCertificatesSettings :: HTTP.ManagerSettings
ignoreTLSCertificatesSettings  = TLS.mkManagerSettings (TLSSettingsSimple True False False) Nothing

test = Text.readFile "vcloudapi.xml"
