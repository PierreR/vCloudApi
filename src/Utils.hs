module Utils where

import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as TLS

import           Network.Wreq
import           Text.Xml.Lens

import           VCloud.Prelude

ignoreTLSCertificatesSettings :: HTTP.ManagerSettings
ignoreTLSCertificatesSettings  = TLS.mkManagerSettings (TLSSettingsSimple True False False) Nothing

-- | Wait for a success status on an action
-- Try it 10 times, then abort
waitTask :: Int -> IO (Response LazyByteString) -> IO ()
waitTask sec action = do
  e <- loop 10 $ do
    liftIO $ putStrLn "Waiting for status"
    liftIO $ threadDelay (sec*1000*1000)
    r <- liftIO action
    case r ^. responseBody .xml.attr "status" of
      Just "success" -> break "Success"
      Just "running" -> continue
      _              -> break "Could not get a status from the server"
  case e of
    Just msg -> putStrLn msg
    Nothing  -> putStrLn "Failure. Didn't receive a success status after 10 attempts "
  where
    continue = empty
    -- asum will strive for the first non empty value
    -- that's why returning one would break the loop
    -- As a note, compare this with forever ... which would have the opposite behavior
    -- `runMaybeT . forever` would break whenever empty is encountered.
    break = return
    loop :: Int -> MaybeT IO String -> IO (Maybe String)
    loop n = runMaybeT . asum . replicate n

test = readFile "vcloudapi.xml"

createXml :: Element -> Document
createXml el = Document simplePrologue el []
  where
    -- '<?xml version=\"1.0\" encoding=\"UTF-8\"?>'
    simplePrologue = Prologue [] Nothing []
