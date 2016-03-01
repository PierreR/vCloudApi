module Utils where

import           Control.Concurrent        (threadDelay)
import           Control.Lens
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy      as LB
import           Data.Foldable
import qualified Data.Text.Lazy.IO         as Text
import           Network.Connection        (TLSSettings (TLSSettingsSimple))
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Client.TLS   as TLS
import           Network.Wreq
import           Text.XML                  (Document (Document),
                                            Prologue (Prologue))
import           Text.Xml.Lens

ignoreTLSCertificatesSettings :: HTTP.ManagerSettings
ignoreTLSCertificatesSettings  = TLS.mkManagerSettings (TLSSettingsSimple True False False) Nothing

-- | Wait for a success status on an action
-- Try it 10 times, then abort
waitTask :: Int -> IO (Response LB.ByteString) -> IO ()
waitTask sec action = do
  e <- loop 10 $ do
    liftIO $ putStrLn "Waiting for status"
    liftIO $ threadDelay (sec*1000*1000)
    r <- liftIO action
    case r ^. responseBody .xml.attr "status" of
      Just "success" -> exit "Success"
      Just "running" -> continue
      _              -> exit "Could not get a status from the server"
  case e of
    Just msg -> putStrLn msg
    Nothing  -> putStrLn "Failure. Didn't receive a success status after 10 attempts "
  where
    continue = empty
    -- asum will strive for the first non empty value
    -- that's why return one would exit the loop
    -- As a note, compare this with forever ... which would have the opposite behavior
    -- `runMaybeT . forever` would exit whenever empty is encountered.
    exit = return
    loop :: Int -> MaybeT IO String -> IO (Maybe String)
    loop n = runMaybeT . asum . replicate n

test = Text.readFile "vcloudapi.xml"

createXml :: Element -> Document
createXml el = Document simplePrologue el []
  where
    -- '<?xml version=\"1.0\" encoding=\"UTF-8\"?>'
    simplePrologue = Prologue [] Nothing []
