{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Basic interface for communication with the ELM327.
module System.Hardware.ELM327.Connection (
  -- * Basic types
  Con(..)
, ConT(..)
, withCon
, ConError(..)

  -- * Interacting with connections
, close
, close'
, send
, sendBytes
, sendString
, recvRaw
, recv
, flushOutputStream

  -- * Sending commands
, at
, obd

  -- * Logging
, fileLog
)
where

import Prelude hiding (log)

import Control.Lens (re, (^.), (^?))
import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)

import Data.ByteString.Char8 (ByteString)
import Data.Char (isHexDigit)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as Char8

import System.IO (openFile, IOMode(..), hPutStrLn, hFlush, hClose)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import System.Hardware.ELM327.Commands (AT,
                                        Command(..),
                                        OBD,
                                        command,
                                        obdMode,
                                        obdPID)
import System.Hardware.ELM327.Errors (OBDError(..),
                                      OBDDecodeError(..),
                                      obdErrorMessage)
import System.Hardware.ELM327.Utils.Hex (hexToBytes)
import System.Hardware.ELM327.Utils.Monad (maybeToExcept, orThrow)

-- | A connection to an ELM327 device that can be closed.
data Con = Con { conInput :: InputStream ByteString   -- ^ Input stream of the connection
               , conOutput :: OutputStream ByteString -- ^ Output stream of the connection
               , conLog :: Maybe ByteString -> IO ()  -- ^ Function that will be called with every byte sent or received, 'Nothing' indicates connection closed.
               }

-- | Monad transformer for connection operations.
newtype ConT m a = ConT { runConT :: ReaderT Con (ExceptT ConError m) a }
                 deriving (Functor, Applicative, Monad, MonadIO, MonadReader Con, MonadError ConError)

-- | Run a 'ConT'
withCon :: Con -> ConT m a -> m (Either ConError a)
withCon con = runExceptT . flip runReaderT con . runConT

-- | Errors that can occur in the 'ConT' monad.
data ConError = ConOBDError OBDError
              | ConTimeoutError
              deriving (Show)

-- | Close the connection
close :: MonadIO m => ConT m ()
close = ask >>= close'


-- | Close the connection
close' :: MonadIO m => Con -> m ()
close' c = do
    liftIO . Streams.write Nothing . conOutput $ c
    liftIO $ conLog c Nothing

-- | Send an ELM327 command.
send :: MonadIO m => Command -> ConT m ()
send cmd = do
    sendString $ cmd ^. re command
    sendString "\r"
    flushOutputStream

-- | Send all bytes to the ELM327.
sendBytes :: MonadIO m => ByteString -> ConT m ()
sendBytes x = ask >>= liftIO . Streams.write (Just x) . conOutput >> void (log $ Just x)

-- | Send a string to the ELM327.
sendString :: MonadIO m => String -> ConT m ()
sendString = sendBytes . Char8.pack

-- | Receive available ELM327 caracters as a byte string
recvRaw :: MonadIO m => ConT m (Maybe ByteString)
recvRaw = ask >>= liftIO . Streams.read . conInput >>= log

-- | Receive an ELM327 response as a byte string.
recv :: MonadIO m => ConT m (Maybe ByteString)
recv = do
    s <- Char8.concat . reverse <$> recv' []
    if Char8.null s then return Nothing else return (Just s)
  where
    recv' xs = do
        bs <- ask >>= liftIO . Streams.read . conInput >>= log
        case bs of Nothing -> throwError ConTimeoutError
                   Just bs' -> handle' xs bs'
    handle' xs bs =
        let (pref, suf) = Char8.break (== '>') . Char8.filter (not . ignored) $ bs in
        if Char8.null suf then recv' (pref:xs)
                          else do ask >>= liftIO . Streams.unRead (Char8.tail suf) . conInput
                                  return (pref:xs)

    ignored x = x == '\r' || x == '\0'

-- | Flush the output stream of a connection.
flushOutputStream :: MonadIO m => ConT m ()
flushOutputStream = ask >>= liftIO. Streams.write (Just "") . conOutput

-- | Send an 'AT' command and expect a response.
at :: MonadIO m => AT -> ConT m (Maybe ByteString)
at cmd = send (AT cmd) >> recv

-- | Send an 'OBD' comand and expect a response.
obd :: MonadIO m => OBD -> ConT m [Word8]
obd cmd = do
    send (OBD cmd)
    mbs <- recv
    case mbs of
        Nothing -> throwError $ ConOBDError (OBDDecodeError EmptyResponseError)
        Just bs -> decode $ Char8.unpack bs
  where
    decode bs = let x = removeStatusPrefixes bs in
                filter isHexDigit <$> maybeToExcept (ConOBDError . OBDErrorMessage <$> x ^? obdErrorMessage) x
                >>= (`orThrow` ConOBDError (OBDDecodeError NotEnoughBytesError)) . hexToBytes
                >>= (`orThrow` ConOBDError (OBDDecodeError NoResponseHeaderError)) . stripHeader

    statusPrefixes = ["SEARCHING..."]
    removeStatusPrefixes x = foldl stripPrefix' x statusPrefixes
    stripPrefix' x pref = fromMaybe x (stripPrefix pref x)
    stripHeader = stripPrefix [0x40 + obdMode cmd, obdPID cmd]

-- | Log sent or received bytes
log :: MonadIO m => Maybe ByteString -> ConT m (Maybe ByteString)
log Nothing = return Nothing
log (Just x) = do
    log' <- conLog <$> ask
    liftIO $ log' (Just x)
    return $ Just x

-- | Create a file logger that can be used with 'conLog'.
fileLog :: FilePath -> IO (Maybe ByteString -> IO ())
fileLog fp = do
    fh <- openFile fp AppendMode
    hPutStrLn fh "Connecting..."
    return $ log' fh
  where
    log' fh Nothing = hClose fh
    log' fh (Just x) = Char8.hPut fh x >> hFlush fh
