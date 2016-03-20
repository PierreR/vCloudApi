{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Option (
    getOption
  , Option(..)
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import           Data.Aeson
import           Options.Generic

import           VCloud.Prelude
import           Utils

data CmdOption = CmdOption
    { zone     :: Maybe FilePath
    , vm     :: Text
    , action   :: String
    } deriving (Show, Generic)

instance ParseRecord CmdOption

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
  CmdOption {..} <- getRecord "VCloud APIs command line wrapper.\nThe configuration is read from a .env file"
  let fp_suffix = maybe mempty (mappend "-") zone
      fp_name = "./.env" <> fp_suffix
  FileConfig {..} <- loadYamlFile fp_name
  return $ Option url user pass appId vm action
