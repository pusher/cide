module Cide.Commands.Init
	( Options(..)
	, run
	) where

import Prelude hiding (FilePath, concat)
import Turtle
import Filesystem.Path (concat)
import Cide.BuildConfig (save, defaultConfig)

data Options = Options
	{ dir :: FilePath }
	deriving (Show, Eq)

run :: Options -> IO ()
run (Options dir') =
	let
		target = concat [dir', ".cide.yml"]
	in
		save target defaultConfig