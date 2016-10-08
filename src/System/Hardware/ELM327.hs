-- | Basic interface for communication with the ELM327.
module System.Hardware.ELM327 where

import qualified Data.ByteString.Char8 as Char8

import System.IO.Streams (makeInputStream, makeOutputStream)

import System.Hardware.Serialport (CommSpeed(..),
                                   FlowControl(..),
                                   Parity(..),
                                   SerialPortSettings(..),
                                   StopBits(..),
                                   closeSerial,
                                   openSerial)
import qualified System.Hardware.Serialport as Port

import System.Hardware.ELM327.Commands (AT(..))
import System.Hardware.ELM327.Connection (Con(..), at)

-- | Connect to an ELM327 device.
connect :: FilePath -> IO Con
connect fp = do
    let s = SerialPortSettings { commSpeed = CS38400
                               , bitsPerWord = 8
                               , stopb = One
                               , parity = NoParity
                               , flowControl = NoFlowControl
                               , timeout = 2 }
    port <- openSerial fp s
    is <- makeInputStream (produce port)
    os <- makeOutputStream (consume port)
    initialize $ Con is os (closeSerial port)
  where
    produce port = Just <$> Port.recv port 8
    consume _    Nothing = return ()
    consume port (Just x) = sendAll port x

    sendAll port bs
        | Char8.null bs = return ()
        | otherwise = do sent <- Port.send port bs
                         sendAll port $ Char8.drop sent bs

    initialize con = do
        _ <- at con ATResetAll
        _ <- at con ATEchoOff
        return con
