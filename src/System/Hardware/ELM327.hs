module System.Hardware.ELM327 where

import Control.Monad (when)
import Control.Lens (re, (^.))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8

import System.Hardware.Serialport (CommSpeed(..),
                                   FlowControl(..),
                                   Parity(..),
                                   SerialPort,
                                   SerialPortSettings(..),
                                   StopBits(..),
                                   closeSerial,
                                   openSerial)
import qualified System.Hardware.Serialport as Port

import System.Hardware.ELM327.Commands (Command, command)

-- | An established connection to an ELM327 device.
newtype Con = Con SerialPort

-- | Connect to an ELM327 device.
connect :: FilePath -> IO Con
connect fp =
    let s = SerialPortSettings { commSpeed = CS38400
                               , bitsPerWord = 8
                               , stopb = One
                               , parity = NoParity
                               , flowControl = NoFlowControl
                               , timeout = 2 } in
    Con <$> openSerial fp s

-- | Close an ELM327 connection.
close :: Con -> IO ()
close (Con port) = closeSerial port

-- | Send an ELM327 command.
send :: Con -> Command -> IO ()
send con cmd = do
    sendString con $ cmd ^. re command
    sendString con "\r"
    flush con

-- | Send all bytes to the ELM327.
sendBytes :: Con -> ByteString -> IO ()
sendBytes con@(Con port) x = do
    n <- Port.send port x
    when (n /= Char8.length x) $ sendBytes con (Char8.drop n x)

-- | Send a string to the ELM327.
sendString :: Con -> String -> IO ()
sendString con = sendBytes con . Char8.pack

-- | Flush the serial connection
flush :: Con -> IO ()
flush (Con port) = Port.flush port
