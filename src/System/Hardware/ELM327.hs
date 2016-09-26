module System.Hardware.ELM327 where

import System.Hardware.Serialport (CommSpeed(..),
                                   FlowControl(..),
                                   Parity(..),
                                   SerialPort,
                                   SerialPortSettings(..),
                                   StopBits(..),
                                   closeSerial,
                                   openSerial)

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
