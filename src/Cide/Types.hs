module Cide.Types where

class Command where
	run :: String -> IO ()

-- import Data.Map

-- data BuildConfig = BuildConfig 
-- 	{ from :: String
-- 	, as_root :: Maybe StepConfig
-- 	, before :: Maybe StepConfig
-- 	, export_dir :: Maybe String
-- 	, use_ssh :: Bool
-- 	, run :: String
-- 	} deriving (Show, Eq)

-- data StepConfig = StepConfig
-- 	{ env :: Map String String
-- 	, add :: Either [String] (Map String String)
-- 	, cmd :: [String]
--     } deriving (Show, Eq)