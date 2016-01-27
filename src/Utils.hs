module Utils where

import qualified Data.Text.Lazy.IO       as Text
import           Network.Connection      (TLSSettings (TLSSettingsSimple))
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import           Text.XML                (Document (Document), Element,
                                          Prologue (Prologue))

ignoreTLSCertificatesSettings :: HTTP.ManagerSettings
ignoreTLSCertificatesSettings  = TLS.mkManagerSettings (TLSSettingsSimple True False False) Nothing

test = Text.readFile "vcloudapi.xml"

createXml :: Element -> Document
createXml root = Document simplePrologue root []
  where
    -- '<?xml version=\"1.0\" encoding=\"UTF-8\"?>'
    simplePrologue = Prologue [] Nothing []
