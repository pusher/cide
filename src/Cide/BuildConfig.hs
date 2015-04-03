{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Cide.BuildConfig where

import Prelude hiding (FilePath)

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Aeson.Types (defaultOptions)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Map (Map, empty)
import Data.Text()
import Data.Yaml (ParseException, decode, decodeFileEither)
import PseudoMacros
--import Control.Lens
import Filesystem (isFile, getWorkingDirectory)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Path

import Control.Monad.Trans (liftIO)

import Generics.Deriving

filename = ".cide.yml"

load :: FilePath -> IO (Either ParseException BuildConfig) 
load path =
  decodeFileEither (Path.encodeString path)

parse :: ByteString -> Maybe BuildConfig
parse = decode

find :: IO (Maybe FilePath)
find = do
  dir <- liftIO $ getWorkingDirectory
  findUp dir
  where
    findUp :: FilePath -> IO (Maybe FilePath)
    findUp dir = do
      let filepath = Path.concat [dir, filename]
      exists <- isFile filepath
      if exists then
        return $ Just filepath
      else
        if dir == Path.root dir then
          return Nothing
        else
          findUp (Path.directory dir)


--

configTemplate = $(embedFile $ $__FILE__ ++ ".yml")

defaultConfig = BuildConfig
  "ubuntu:14.04"
  emptyBuildstep emptyBuildstep emptyBuildstep
  "artifacts" False "script/ci"

emptyBuildstep = BuildStep empty (Left []) []

--

data BuildConfig = BuildConfig 
  { from :: String
  , bootstrap :: BuildStep
  , as_root :: BuildStep
  , before :: BuildStep
  , export_dir :: String
  , ssh_key :: Bool
  , run :: String
  } deriving (Show, Eq, Generic)

instance ToJSON BuildConfig where
  toJSON = genericToJSON defaultOptions

instance FromJSON BuildConfig where
  parseJSON = genericParseJSON defaultOptions

--

data BuildStep = BuildStep
  { env :: Map String String
  , add :: Either [String] (Map String String)
  , cmd :: [String]
  } deriving (Show, Eq, Generic)

instance ToJSON BuildStep where
  toJSON = genericToJSON defaultOptions

instance FromJSON BuildStep where
  parseJSON = genericParseJSON defaultOptions

