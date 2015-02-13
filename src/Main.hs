module Main where

import Paths_cide (version)
import Data.Version (showVersion)

import Cide.Options
-- import Cide.Build
import Cide.Logging
-- import Cide.Types

-- import Control.Monad
import System.Exit
-- import System.Directory
import System.Process

runProg :: Options -> IO ()
runProg (Options logLevel cmd) = do
    initLoggingFramework logLevel
    ec <- system "command -v docker >/dev/null 2>&1"
    case ec of
        ExitSuccess ->
            runProg' cmd
        ExitFailure _ ->
            error "cide requires docker. Install from http://docker.com"

runProg' :: Command -> IO ()
runProg' cmd =
    case cmd of
        BuildCommand options ->
            putStrLn $ "building with " ++ show options
        CleanCommand options ->
            putStrLn $ "cleaning with " ++ show options
        InitCommand  ->
            putStrLn "My kitchen is ready for cideing!"
        VersionCommand ->
            putStrLn $ "cide v" ++ showVersion version

main :: IO ()
main = withOptions runProg
