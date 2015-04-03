module Cide.Commands.Init
  ( Options(..)
  , run
  ) where

import Prelude hiding (FilePath, concat, writeFile)
import Turtle
import Filesystem (writeFile)
import Filesystem.Path.CurrentOS (concat)
import Cide.BuildConfig (configTemplate)

data Options = Options
  { dir :: FilePath }
  deriving (Show, Eq)

run :: Options -> IO ()
run (Options dir') =
  let
  target = concat [dir', ".cide.yml"]
  in
  writeFile target configTemplate
