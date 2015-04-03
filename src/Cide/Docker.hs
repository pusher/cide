module Cide.Docker where

import System.Cmd (system)
import System.Process (readProcess)
import Text.Regexp (mkRegex, matchRegex)

data DockerEnv =
  DockerEnv {
    env :: [(String, String)]
  } deriving (Show)

withDocker :: IO (DockerEnv)
#if defined(CABAL_OS_DARWIN)
withDocker = checkDocker setupWithBoot2docker
#else
withDocker = checkDocker
#endif

checkDocker nextStep = do
  ec <- system "which docker >/dev/null"
  case ec of
    ExitSuccess ->
      DockerEnv []
    ExitFailure _ ->
      error "cide requires docker. Install from http://docker.com"

envRegex = mkRegex "export (\w+)=(.*)"

setupWithBoot2docker nextStep = do
  dockerHost <- getEnv("DOCKER_HOST")
  if dockerHost == ""
    ec <- system "which boot2docker >/dev/null"
    case ec of
      ExitSuccess ->
        setupDocker
      ExitFailure ->
        error "cide requires boot2docker on OSX to work. Install from http://boot2docker.io"
  where
  setupDocker = do
    envStr <- readProcess "boot2docker" ["shellinit"] []
    DockerEnv $ map kv $ lines envStr

  kv str =
    Just [k,v] = matchRegex envRegex str
    (k, v)
