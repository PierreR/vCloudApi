{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Option (
    getOption
  , Option(..)
) where

import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as B

import           Data.Aeson
import           Options.Applicative
import           Text.PrettyPrint.ANSI.Leijen as Exports hiding ((<$>), (<>))

import           VCloud.Prelude
import           Utils

-- Argument parser
data CmdOption = CmdOption
    { zone   :: Maybe FilePath
    , vm     :: Text
    , action :: String
    } deriving Show

argParser :: Parser CmdOption
argParser = CmdOption
    <$> optional (strOption
        ( long "zone"
        <> help "Which config file to read (.env-$zone)"))
    <*> ( Text.pack <$> strOption
        ( long "vm"
        <> help "The name of the virtual machine target"))
    <*> strOption
        ( long "action"
        <> help "virtual machine action to perform against the API (ex: revertToCurrentSnapshot)")

cmdOpts :: IO CmdOption
cmdOpts = execParser $ info (helper <*> argParser)
        ( fullDesc
        <> progDescDoc (Just (string "VCloud APIs command line wrapper." <> line <>
                              dullyellow "Please configure the 'url', 'user', 'pass' and 'appId' in a local yaml file" <> line <>
                              dullyellow "The file is named '.env' or '.env-$zone' when a zone is specified."))
        )

-- File config parser
data FileConfig = FileConfig
    { url  :: String
    , user :: Text
    , pass :: Text
    , appId     :: String
    } deriving (Show, Generic, FromJSON)

data Option = Option
    { vCloudURL  :: String
    , vCloudUser :: Text
    , vCloudPass :: Text
    , vAppId     :: String
    , vmName :: Text
    , vmAction :: String
    } deriving (Show, Generic)

getOption :: IO Option
getOption = do
  CmdOption {..} <- cmdOpts
  let fp_suffix = maybe mempty (mappend "-") zone
      fp_name = "./.env" <> fp_suffix
  FileConfig {..} <- loadYamlFile fp_name
  return $ Option url user pass appId vm action
