module Option where

import           Data.ByteString.Char8 (ByteString, pack)
import           Options.Applicative

data Options = Options
    { vCloudUser :: ByteString
    , vCloudPass :: ByteString
    , vAppId     :: String
    }

options :: Parser Options
options = Options
    <$> ( pack <$> strOption
        (  long "user"
        <> short 'u'
        <> help "Your user name and org ex.: myuser@cirb-test"))
    <*> ( pack <$> strOption
        (  long "password"
        <> short 'p'
        <> help "Your password"))
    <*> strOption
        (long "vAppId"
        <> short 'q'
        <> help "vApp id to query for (default is 'f1ce1ecb-af39-41d3-ac9c-21deecb4e23d')"
        <> value "f1ce1ecb-af39-41d3-ac9c-21deecb4e23d")

cmdOpts :: IO Options
cmdOpts = execParser $ info (helper <*> options)
        (fullDesc
         <> progDesc "VCloud APIs command line wrapper"
        )
