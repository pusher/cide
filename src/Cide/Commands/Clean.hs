module Cide.Commands.Clean
	( Options(..)
	, run
	) where

data Options = Options
    { days :: Integer
    , count :: Integer
    }
    deriving (Show, Eq)

run :: Options -> IO ()
run _ = putStrLn "Hello"