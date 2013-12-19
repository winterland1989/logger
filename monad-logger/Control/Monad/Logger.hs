{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
-- |  This module provides the facilities needed for a decoupled logging system.
--
-- The 'MonadLogger' class is implemented by monads that give access to a
-- logging facility.  If you're defining a custom monad, then you may define an
-- instance of 'MonadLogger' that routes the log messages to the appropriate
-- place (e.g., that's what @yesod-core@'s @GHandler@ does).  Otherwise, you
-- may use the 'LoggingT' monad included in this module (see
-- 'runStderrLoggingT'). To simply discard log message, use 'NoLoggingT'.
--
-- As a user of the logging facility, we provide you some convenient Template
-- Haskell splices that use the 'MonadLogger' class.  They will record their
-- source file and position, which is very helpful when debugging.  See
-- 'logDebug' for more information.
module Control.Monad.Logger
    ( -- * MonadLogger
      MonadLogger
    , monadLoggerLog
    , askLogFunc
    , LogLevel(..)
    , HasLogFunc (..)
    , LogSource
    -- * Helper transformer
    , runStderrLoggingT
    , runStdoutLoggingT
    , runNoLoggingT
    , withChannelLogger
    , LogFunc
    , defaultLogFunc
    -- * TH logging
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logOther
    -- * TH logging with source
    , logDebugS
    , logInfoS
    , logWarnS
    , logErrorS
    , logOtherS
    -- * TH util
    , liftLoc
    -- * Non-TH logging
    , logDebugN
    , logInfoN
    , logWarnN
    , logErrorN
    , logOtherN
    -- * Non-TH logging with source
    , logDebugNS
    , logInfoNS
    , logWarnNS
    , logErrorNS
    , logOtherNS
    ) where

import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)
import System.Log.FastLogger (LogStr, pushLogStr, ToLogStr (toLogStr), LoggerSet, newLoggerSet, defaultBufSize)
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid (mempty, mappend)
import qualified GHC.IO.FD as FD

import Control.Applicative (Const (..))
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Exception.Lifted
import Control.Monad (when, void, liftM)
import Control.Monad.Loops (untilM)
import Control.Monad.Trans.Control (MonadBaseControl (..))

import Control.Monad.IO.Class (MonadIO (liftIO))

import Control.Monad.Trans.Reader   ( ReaderT (..) )

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import Control.Monad.Reader.Class ( MonadReader (..) )

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther Text
    deriving (Eq, Prelude.Show, Prelude.Read, Ord)

instance Lift LogLevel where
    lift LevelDebug = [|LevelDebug|]
    lift LevelInfo = [|LevelInfo|]
    lift LevelWarn = [|LevelWarn|]
    lift LevelError = [|LevelError|]
    lift (LevelOther x) = [|LevelOther $ pack $(lift $ unpack x)|]

type LogSource = Text

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type MonadLogger env m = (MonadIO m, MonadReader env m, HasLogFunc env)

monadLoggerLog :: MonadLogger env m => Loc -> LogSource -> LogLevel -> LogStr -> m ()
monadLoggerLog a b c d = do
    env <- ask
    liftIO $ getConst (logFunc Const env) a b c d

askLogFunc :: (MonadReader env m, HasLogFunc env) => m LogFunc
askLogFunc = (getConst . logFunc Const) `liftM` ask

class HasLogFunc a where
    logFunc :: forall f. Functor f => (LogFunc -> f LogFunc) -> a -> f a
instance HasLogFunc LogFunc where
    logFunc = id

logTH :: LogLevel -> Q Exp
logTH level =
    [|monadLoggerLog $(qLocation >>= liftLoc) (pack "") $(lift level) . (toLogStr :: Text -> LogStr)|]

-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebug) "This is a debug log message"
logDebug :: Q Exp
logDebug = logTH LevelDebug

-- | See 'logDebug'
logInfo :: Q Exp
logInfo = logTH LevelInfo
-- | See 'logDebug'
logWarn :: Q Exp
logWarn = logTH LevelWarn
-- | See 'logDebug'
logError :: Q Exp
logError = logTH LevelError

-- | Generates a function that takes a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $(logOther "My new level") "This is a log message"
logOther :: Text -> Q Exp
logOther = logTH . LevelOther

-- | Lift a location into an Exp.
--
-- Since 0.3.1
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(lift a)
    $(lift b)
    $(lift c)
    ($(lift d1), $(lift d2))
    ($(lift e1), $(lift e2))
    |]

-- | Generates a function that takes a 'LogSource' and 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $logDebugS "SomeSource" "This is a debug log message"
logDebugS :: Q Exp
logDebugS = [|\a -> monadLoggerLog $(qLocation >>= liftLoc) a LevelDebug . (toLogStr :: Text -> LogStr)|]

-- | See 'logDebugS'
logInfoS :: Q Exp
logInfoS = [|\a -> monadLoggerLog $(qLocation >>= liftLoc) a LevelInfo . (toLogStr :: Text -> LogStr)|]
-- | See 'logDebugS'
logWarnS :: Q Exp
logWarnS = [|\a -> monadLoggerLog $(qLocation >>= liftLoc) a LevelWarn . (toLogStr :: Text -> LogStr)|]
-- | See 'logDebugS'
logErrorS :: Q Exp
logErrorS = [|\a -> monadLoggerLog $(qLocation >>= liftLoc) a LevelError . (toLogStr :: Text -> LogStr)|]

-- | Generates a function that takes a 'LogSource', a level name and a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $logOtherS "SomeSource" "My new level" "This is a log message"
logOtherS :: Q Exp
logOtherS = [|\src level -> monadLoggerLog $(qLocation >>= liftLoc) src (LevelOther level) . (toLogStr :: Text -> LogStr)|]

defaultLogFunc :: LoggerSet -> LogFunc
defaultLogFunc ls a b c d = pushLogStr ls (defaultRender a b c d)

defaultRender :: Loc -> LogSource -> LogLevel -> LogStr -> LogStr
defaultRender loc src level msg =
    "[" `mappend`
    (case level of
        LevelOther t -> toLogStr t
        _ -> toLogStr $ S8.pack $ drop 5 $ show level) `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend`
    msg `mappend`
    (if isDefaultLoc loc
        then "\n"
        else
            " @(" `mappend`
            toLogStr (S8.pack fileLocStr) `mappend`
            ")\n")
  where
    -- taken from file-location package
    -- turn the TH Loc loaction information into a human readable string
    -- leaving out the loc_end parameter
    fileLocStr = (loc_package loc) ++ ':' : (loc_module loc) ++
      ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

stdout, stderr :: LoggerSet
stdout = unsafePerformIO $ newLoggerSet defaultBufSize FD.stdout
{-# NOINLINE stdout #-}
stderr = unsafePerformIO $ newLoggerSet defaultBufSize FD.stderr
{-# NOINLINE stderr #-}

-- | Run a block using a @MonadLogger@ instance which prints to stderr.
--
-- Since 0.2.2
runStderrLoggingT :: ReaderT LogFunc m a -> m a
runStderrLoggingT = (`runReaderT` defaultLogFunc stderr)

-- | Run a block using a @MonadLogger@ instance which prints to stdout.
--
-- Since 0.2.2
runStdoutLoggingT :: ReaderT LogFunc m a -> m a
runStdoutLoggingT = (`runReaderT` defaultLogFunc stdout)

runNoLoggingT :: ReaderT LogFunc m a -> m a
runNoLoggingT = (`runReaderT` (\_ _ _ _ -> return ()))

-- | Within the 'LoggingT' monad, capture all log messages to a bounded
--   channel of the indicated size, and only actually log them if there is an
--   exception.
--
-- Since 0.3.2
withChannelLogger :: (MonadBaseControl IO m, MonadIO m)
                  => Int         -- ^ Number of mesasges to keep
                  -> ReaderT LogFunc m a
                  -> ReaderT LogFunc m a
withChannelLogger size action = ReaderT $ \logger -> do
    chan <- liftIO $ newTBChanIO size
    runReaderT action (channelLogger chan logger) `onException` dumpLogs chan
  where
    channelLogger chan logger loc src lvl str = atomically $ do
        full <- isFullTBChan chan
        when full $ void $ readTBChan chan
        writeTBChan chan $ logger loc src lvl str

    dumpLogs chan = liftIO $
        sequence_ =<< atomically (untilM (readTBChan chan) (isEmptyTBChan chan))

defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc a b c d e) =
    (a == v) &&
    (b == w) &&
    (c == x) &&
    (d == y) &&
    (e == z)
  where
    Loc v w x y z = defaultLoc

logDebugN :: MonadLogger env m => Text -> m ()
logDebugN =
    monadLoggerLog defaultLoc "" LevelDebug . toLogStr

logInfoN :: MonadLogger env m => Text -> m ()
logInfoN =
    monadLoggerLog defaultLoc "" LevelInfo . toLogStr

logWarnN :: MonadLogger env m => Text -> m ()
logWarnN =
    monadLoggerLog defaultLoc "" LevelWarn . toLogStr

logErrorN :: MonadLogger env m => Text -> m ()
logErrorN =
    monadLoggerLog defaultLoc "" LevelError . toLogStr

logOtherN :: MonadLogger env m => LogLevel -> Text -> m ()
logOtherN level =
    monadLoggerLog defaultLoc "" level . toLogStr

logDebugNS :: MonadLogger env m => Text -> Text -> m ()
logDebugNS src =
    monadLoggerLog defaultLoc src LevelDebug . toLogStr

logInfoNS :: MonadLogger env m => Text -> Text -> m ()
logInfoNS src =
    monadLoggerLog defaultLoc src LevelInfo . toLogStr

logWarnNS :: MonadLogger env m => Text -> Text -> m ()
logWarnNS src =
    monadLoggerLog defaultLoc src LevelWarn . toLogStr

logErrorNS :: MonadLogger env m => Text -> Text -> m ()
logErrorNS src =
    monadLoggerLog defaultLoc src LevelError . toLogStr

logOtherNS :: MonadLogger env m => Text -> LogLevel -> Text -> m ()
logOtherNS src level =
    monadLoggerLog defaultLoc src level . toLogStr
