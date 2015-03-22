module Cide.Commands.Version
	( run
	) where

import Paths_cide (version)
import Data.Version (showVersion)

run :: IO ()
run = putStrLn $ "cide v" ++ showVersion version