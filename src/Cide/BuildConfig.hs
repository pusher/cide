{-# LANGUAGE DeriveGeneric #-}
module Cide.BuildConfig where

import Prelude hiding (FilePath)

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Aeson.Types (defaultOptions)
import Data.Map
import Data.Yaml (ParseException, encodeFile, decodeFileEither)
import Data.Text()

import Filesystem.Path.CurrentOS (FilePath, encodeString)
import Generics.Deriving

load :: FilePath -> IO (Either ParseException BuildConfig) 
load path =
	decodeFileEither (encodeString path)


save :: FilePath -> BuildConfig -> IO ()
save path =
	encodeFile (encodeString path)

--

defaultConfig = BuildConfig "ubuntu:14.04" Nothing Nothing Nothing False "./script/ci"

--

data BuildConfig = BuildConfig 
	{ from :: String
	, as_root :: Maybe StepConfig
	, before :: Maybe StepConfig
	, export_dir :: Maybe String
	, use_ssh :: Bool
	, run :: String
	} deriving (Show, Eq, Generic)

instance ToJSON BuildConfig where
    toJSON = genericToJSON defaultOptions

instance FromJSON BuildConfig where
    parseJSON = genericParseJSON defaultOptions

--

data StepConfig = StepConfig
	{ env :: Map String String
	, add :: Either [String] (Map String String)
	, cmd :: [String]
    } deriving (Show, Eq, Generic)

instance ToJSON StepConfig where
    toJSON = genericToJSON defaultOptions

instance FromJSON StepConfig where
    parseJSON = genericParseJSON defaultOptions

