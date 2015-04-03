module Main where

import Cide.Options
import Cide.Logging

import System.Exit
import System.Process

import qualified Cide.Commands.Build as Build
import qualified Cide.Commands.Clean as Clean
import qualified Cide.Commands.Init as Init
import qualified Cide.Commands.Version as Version


runProg :: Options -> IO ()
runProg (Options logLevel cmd) = do
  initLoggingFramework logLevel
  ec <- system "which docker >/dev/null"
  case ec of
    ExitSuccess ->
      runProg' cmd
    ExitFailure _ ->
      error "cide requires docker. Install from http://docker.com"

runProg' :: Command -> IO ()
runProg' cmd =
  case cmd of
    BuildCommand options ->
      Build.run options
    CleanCommand options ->
      Clean.run options
    InitCommand options ->
      Init.run options
    VersionCommand ->
      Version.run

main :: IO ()
main = withOptions runProg
