-- | Basic interface for communication with the ELM327.
module System.Hardware.ELM327.Connection where

import Control.Lens (re, (^.), (^?))
import Data.ByteString.Char8 (ByteString)
import Data.Char (isHexDigit)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as Char8

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
import System.Hardware.ELM327.Utils.Monad (maybeToLeft, maybeToRight)

-- | A connection to an ELM327 device that can be closed.
data Con = Con (InputStream ByteString) (OutputStream ByteString) (IO ())

-- | Close the connection
close :: Con -> IO ()
close (Con _ _ c) = c

-- | Send an ELM327 command.
send :: Con -> Command -> IO ()
send con cmd = do
    sendString con $ cmd ^. re command
    sendString con "\r"
    flushOutputStream con

-- | Send all bytes to the ELM327.
sendBytes :: Con -> ByteString -> IO ()
sendBytes (Con _ os _) x = Streams.write (Just x) os

-- | Send a string to the ELM327.
sendString :: Con -> String -> IO ()
sendString con = sendBytes con . Char8.pack

-- | Receive available ELM327 caracters as a byte string
recvRaw :: Con -> IO (Maybe ByteString)
recvRaw (Con is _ _) = Streams.read is

-- | Receive an ELM327 response as a byte string.
recv :: Con -> IO (Maybe ByteString)
recv (Con is _ _) = do
    s <- Char8.concat <$> recv' []
    if Char8.null s then return Nothing else return (Just s)
  where
    recv' xs = do
        bs <- Streams.read is
        case bs of Nothing -> recv' xs
                   Just bs' -> handle' xs bs'
    handle' xs bs =
        let (pref, suf) = Char8.break (== '>') . Char8.filter (not . ignored) $ bs in
        if Char8.null suf then recv' (pref:xs)
                          else Streams.unRead (Char8.tail suf) is >> return (pref:xs)

    ignored x = x == '\r' || x == '\0'

-- | Flush the output stream of a connection.
flushOutputStream :: Con -> IO ()
flushOutputStream (Con _ os _) = Streams.write (Just "") os

-- | Send an 'AT' command and expect a response.
at :: Con -> AT -> IO (Maybe ByteString)
at con cmd = send con (AT cmd) >> recv con

-- | Send an 'OBD' comand and expect a response.
obd :: Con -> OBD -> IO (Either OBDError [Word8])
obd con cmd = do
    send con (OBD cmd)
    mbs <- recv con
    case mbs of
        Nothing -> return $ Left (OBDDecodeError EmptyResponseError)
        Just bs -> return . decode $ Char8.unpack bs
  where
    decode bs = Right (removeStatusPrefixes bs)
                >>= \x -> maybeToLeft x (OBDErrorMessage <$> x ^? obdErrorMessage)
                >>= Right . filter isHexDigit
                >>= maybeToRight (OBDDecodeError NotEnoughBytesError) . hexToBytes
                >>= maybeToRight (OBDDecodeError NoResponseHeaderError) . stripHeader

    statusPrefixes = ["SEARCHING..."]
    removeStatusPrefixes x = foldl stripPrefix' x statusPrefixes
    stripPrefix' x pref = fromMaybe x (stripPrefix pref x)
    stripHeader = stripPrefix [0x40 + obdMode cmd, obdPID cmd]
