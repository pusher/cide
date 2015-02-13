module Cide.Logging
    ( initLoggingFramework
    , logInfo
    , logDebug
    , logWarn
    , logError
    ) where

import Control.Monad.Trans

import System.IO (stdout)
import System.Log.Formatter
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Logger

initLoggingFramework :: Priority -> IO ()
initLoggingFramework prio = do
    myStreamHandler <- streamHandler stdout prio
    let myStreamHandler' = setFormatter myStreamHandler (simpleLogFormatter "[$prio $time $loggername] $msg")
    root <- getRootLogger
    saveGlobalLogger (setLevel DEBUG $ setHandlers [myStreamHandler'] root)

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . infoM "cide"

logDebug :: MonadIO m => String -> m ()
logDebug = liftIO . debugM "cide"

logWarn :: MonadIO m => String -> m ()
logWarn = liftIO . warningM "cide"

logError :: MonadIO m => String -> m ()
logError = liftIO . errorM "cide"
