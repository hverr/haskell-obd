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
import System.Hardware.ELM327.Connection (Con(..), ConError, withCon, at)

-- | Connect to an ELM327 device.
connect :: FilePath -> IO (Either ConError Con)
connect fp = do
    let s = SerialPortSettings { commSpeed = CS38400
                               , bitsPerWord = 8
                               , stopb = One
                               , parity = NoParity
                               , flowControl = NoFlowControl
                               , timeout = 5 {- 500ms -} }
    port <- openSerial fp s
    is <- makeInputStream (produce port)
    os <- makeOutputStream (consume port)
    initialize $ Con is os
  where
    produce port = do
        b <- Port.recv port 8
        if Char8.null b then return Nothing
                        else return $ Just b
    consume port Nothing = closeSerial port
    consume port (Just x) = sendAll port x

    sendAll port bs
        | Char8.null bs = return ()
        | otherwise = do sent <- Port.send port bs
                         sendAll port $ Char8.drop sent bs

    initialize con = do
        v <- withCon con $ do
            _ <- at ATResetAll
            _ <- at ATEchoOff
            return ()
        return (v >> return con)
