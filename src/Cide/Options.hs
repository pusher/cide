module Cide.Options
    (withOptions
    , Options(..)
    , Command(..)
    , BuildOptions(..)
    , CleanOptions(..)
    ) where

-- import Cide.Types
import Options.Applicative
import System.Log.Logger (Priority( INFO ))

data Options = Options Priority Command

data Command
    = BuildCommand BuildOptions
    | CleanCommand CleanOptions
    | InitCommand
    | VersionCommand
    deriving (Show, Eq)

data BuildOptions = BuildOptions
    { name :: String
    , hostExportDir :: FilePath
    , export :: Bool
    , sshKey :: FilePath
    }
    deriving (Show, Eq)

data CleanOptions = CleanOptions
    { days :: Integer
    , count :: Integer
    }
    deriving (Show, Eq)

buildNameP =
    strOption $
    long "name" <>
    short 'n' <>
    metavar "NAME" <>
    value "cide" <>
    help "Name of the build"

buildExportDirP :: Parser FilePath
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

buildSSHKeyP :: Parser FilePath
buildSSHKeyP =
    strOption $
    long "ssh_key" <>
    short 's' <>
    metavar "PATH" <>
    value "~/.ssh/id_rsa" <>
    help "Path to a ssh key to import into the docker image"

cideVerboseP :: Parser Priority
cideVerboseP =
    option auto $
    long "verbosity" <>
    short 'v' <>
    value INFO <>
    help "log levels"

parseBuild :: Parser Command
parseBuild = BuildCommand <$>
    (BuildOptions
        <$> buildNameP
        <*> buildExportDirP
        <*> buildExportP
        <*> buildSSHKeyP
        )

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

parseClean :: Parser Command
parseClean = CleanCommand <$>
    (CleanOptions
        <$> cleanDaysP
        <*> cleanCountP
        )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand = subparser $
    command "build"   (parseBuild          `withInfo` "Builds an image and executes the run script") <>
    command "clean"   (parseClean          `withInfo` "Removes old containers") <>
    command "version" (pure VersionCommand `withInfo` "Show programs version") <>
    command "init"    (pure InitCommand    `withInfo` "Enable cide for current project / directory")

parseOptions :: Parser Options
parseOptions =
    Options <$> cideVerboseP <*> parseCommand

withOptions :: (Options -> IO ()) -> IO ()
withOptions f = f =<< execParser
    (parseOptions `withInfo` "Continuous Integration Docker Environment")
