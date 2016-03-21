{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Option (
    option
  , Option(..)
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import           Data.Aeson
import           Options.Generic

import           VCloud.Prelude
import           Utils

data Option' = Option'
    { zone   :: Maybe String
    , url    :: Maybe String
    , user   :: Maybe Text
    , pass   :: Maybe Text
    , appId  :: Maybe String
    , vm     :: Maybe Text
    , action :: Maybe String
    } deriving (Eq, Ord, Show, Generic)

instance ParseRecord Option'
instance FromJSON Option'

instance Monoid Option' where
   mempty  = mempty
   mappend (Option' a b c d e f g)
           (Option' a' b' c' d' e' f' g') = Option' (a<|>a') (b<|>b') (c<|>c') (d<|>d') (e<|>e') (f<|>f') (g<|>g')

data Option = Option
    { vCloudURL  :: String
    , vCloudUser :: Text
    , vCloudPass :: Text
    , vAppId     :: String
    , vmName     :: Text
    , vmAction   :: String
    } deriving (Eq, Show)

option :: IO Option
option = do
  cmd_opt <- getRecord "VCloud APIs command line wrapper.\nThe configuration is read from a .env file"
  let fp_suffix = "" --maybe mempty (mappend "-") (zone cmd_opt)
      fp_name = "./.env" <> fp_suffix
  file_opt <- loadYamlFile fp_name
  let Option' {..} = cmd_opt <> file_opt
      msg x = "Field '" <> x <> "' not set either on the command line or inside '" <> fp_name <> "' config file."
  -- return $ Option mempty mempty mempty mempty mempty mempty
  Option <$> maybe (error $ msg "url") return url
         <*> maybe (error $ msg "user") return user
         <*> maybe (error $ msg "pass") return pass
         <*> maybe (error $ msg "appId") return appId
         <*> maybe (error $ msg "vm") return vm
         <*> maybe (error $ msg "action") return action
