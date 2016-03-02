module Option where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as Text

import           Options.Applicative

import           VCloud.Prelude

data Options = Options
    { vCloudURL  :: String
    , vCloudUser :: ByteString
    , vCloudPass :: ByteString
    , vAppId     :: String
    , vmName     :: Text
    , vmAction   :: String
    }

options :: Parser Options
options = Options
    <$> strOption
        ( long "vCloudURL"
        <> short 'h'
        <> help "VCloud base URL"
        <> value "https://c.irisnet.be")
    <*> ( BC.pack <$> strOption
        ( long "user"
        <> short 'u'
        <> help "Your user name and org ex.: myuser@cirb-test"))
    <*> ( BC.pack <$> strOption
        ( long "password"
        <> short 'p'
        <> help "Your password"))
    <*> strOption
        ( long "vAppId"
        <> short 'q'
        <> help "vApp id (such as 'f1ce1ecb-af39-41d3-ac9c-21deecb4e23d')")
    <*> ( Text.pack <$> strOption
        ( long "vmName"
        <> short 'n'
        <> help "virtual machine name (such as 'saltsyndic')"))
    <*> strOption
        ( long "vmAction"
        <> short 'a'
        <> help "virtual machine action to perform against the API (ex: revertToCurrentSnapshot)")

cmdOpts :: IO Options
cmdOpts = execParser $ info (helper <*> options)
        ( fullDesc
        <> progDesc "VCloud APIs command line wrapper"
        )
