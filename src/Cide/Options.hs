module Cide.Options
  (withOptions
  , Options(..)
  , Command(..)
  ) where

-- import Cide.Types
import Prelude hiding (FilePath)
import Options.Applicative
import System.Log.Logger (Priority( INFO ))
import Filesystem.Path.CurrentOS (decode)
import qualified Data.Text as T

import qualified Cide.Commands.Build as Build
import qualified Cide.Commands.Clean as Clean
import qualified Cide.Commands.Init as Init

data Options = Options Priority Command

data Command
  = BuildCommand Build.Options
  | CleanCommand Clean.Options
  | InitCommand Init.Options
  | VersionCommand
  deriving (Show, Eq)

parseBuild :: Parser Command
parseBuild = BuildCommand <$>
  (Build.Options
    <$> buildNameP
    <*> (fmap (decode . T.pack) buildExportDirP)
    <*> buildExportP
    <*> (fmap (decode . T.pack) buildSSHKeyP)
    )
  where
    buildNameP =
      strOption $
      long "name" <>
      short 'n' <>
      metavar "NAME" <>
      value "cide" <>
      help "Name of the build"

    buildExportDirP =
      strOption $
      long "host_export_dir" <>
      short 'o' <>
      metavar "PATH" <>
      value "." <>
      help "Output directory on host to put build artefacts in"

    buildExportP =
      switch $
      long "export" <>
      short 'e' <>
      help "Are we expecting to export artifacts"

    buildSSHKeyP =
      strOption $
      long "ssh_key" <>
      short 's' <>
      metavar "PATH" <>
      value "~/.ssh/id_rsa" <>
      help "Path to a ssh key to import into the docker image"



parseClean :: Parser Command
parseClean = CleanCommand <$>
  (Clean.Options
    <$> cleanDaysP
    <*> cleanCountP
    )
  where
    cleanDaysP =
      option auto $
      long "days" <>
      metavar "INT" <>
      value 7 <>
      help "Number of days to keep the images"

    cleanCountP =
      option auto $
      long "count" <>
      metavar "INT" <>
      value 10 <>
      help "Maximum number of images to keep"


parseInit :: Parser Command
parseInit = InitCommand <$>
  (Init.Options <$> (fmap (decode . T.pack) initDirP))
  where
    initDirP =
      option auto $
      long "dir" <>
      metavar "PATH" <>
      value "." <>
      help "Project root directory"


parseVersion :: Parser Command
parseVersion = pure VersionCommand


withOptions :: (Options -> IO ()) -> IO ()
withOptions f = f =<< execParser
  (parseOptions `withInfo` "Continuous Integration Docker Environment")
  where
    withInfo :: Parser a -> String -> ParserInfo a
    withInfo opts desc = info (helper <*> opts) $ progDesc desc

    parseCommand :: Parser Command
    parseCommand = subparser $
      command "build"   (parseBuild   `withInfo` "Builds an image and executes the run script") <>
      command "clean"   (parseClean   `withInfo` "Removes old containers") <>
      command "init"  (parseInit  `withInfo` "Enable cide for current project / directory") <>
      command "version" (parseVersion `withInfo` "Show programs version")

    parseOptions :: Parser Options
    parseOptions =
      Options <$> cideVerboseP <*> parseCommand
      where
        cideVerboseP :: Parser Priority
        cideVerboseP =
          option auto $
          long "verbosity" <>
          short 'v' <>
          value INFO <>
          help "log levels"
