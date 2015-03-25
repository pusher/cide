module Cide.Docker where

data DockerEnv = DockerEnv

setup :: IO (DockerEnv)
#if defined(CABAL_OS_DARWIN)
setup = 
#else
setup = 
#endif