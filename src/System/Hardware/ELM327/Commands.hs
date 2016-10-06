-- | A list of supported ELM327 commands (incomplete).
module System.Hardware.ELM327.Commands where

import Control.Lens (Prism', prism', re, (^.), (^?))
import Data.Bits (shiftR, (.&.), (.|.))
import Data.Word (Word8, Word16)
import Numeric (readHex)
import Text.Printf (printf)

-- | A generic command, that can be sent to an ELM327.
data Command = AT AT
             | OBD OBD
             deriving (Eq, Show)

-- | A prism between 'String' and 'Command'
command :: Prism' String Command
command = prism' conv mConv
  where
    conv (AT x) = "AT" ++ x ^. re at
    conv (OBD x) = x ^. re obd

    mConv ('A':'T':xs) = AT <$> xs ^? at
    mConv xs = OBD <$> xs ^? obd

-- | An AT command, to configure the ELM327.
data AT = ATDescribeProtocolNumber
        | ATEchoOff
        | ATReadVoltage
        | ATResetAll
        | ATSelectProtocol Protocol
        | ATVersionID
        deriving (Eq, Show)

-- | A prism between 'String' and 'AT'.
at :: Prism' String AT
at = prism' conv mConv
  where
    conv ATDescribeProtocolNumber = "DPN"
    conv ATEchoOff = "E0"
    conv ATReadVoltage = "RV"
    conv ATResetAll = "Z"
    conv (ATSelectProtocol p) = "SP" ++ p ^. re protocol
    conv ATVersionID = "I"

    mConv "DPN" = Just ATDescribeProtocolNumber
    mConv "E0" = Just ATEchoOff
    mConv "RV" = Just ATReadVoltage
    mConv "Z" = Just ATResetAll
    mConv ('S':'P':p) = ATSelectProtocol <$> p ^? protocol
    mConv "I" = Just ATVersionID
    mConv _ = Nothing

-- | An OBD command, to talk to the vehicle.
--
-- Every OBD command can be one of several modes.
data OBD = CurrentData CurrentData
         deriving (Eq, Show)

-- | Return the mode of an 'OBD' command.
obdMode :: OBD -> Word8
obdMode cmd = word16Mode $ cmd ^. re obdWord16

-- | Return the PID of an 'OBD' command.
obdPID :: OBD -> Word8
obdPID cmd = word16PID $ cmd ^. re obdWord16

-- | Return the mode of an 'OBD' command represented as a 'Word16'.
word16Mode :: Word16 -> Word8
word16Mode x = fromIntegral $ shiftR x 8 .&. 0xFF

-- | Return the PID of an 'OBD' command represented as a 'Word16'.
word16PID :: Word16 -> Word8
word16PID x = fromIntegral $ x .&. 0xFF

-- | A prism between 'OBD' and 'Word16'
obdWord16 :: Prism' Word16 OBD
obdWord16 = prism' conv mConv
  where
    conv (CurrentData x) = 0x0100 .|. fromIntegral (x ^. re currentData)

    mConv x | word16Mode x == 0x01 = CurrentData <$> word16PID x ^? currentData
            | otherwise = Nothing

-- | A prism between 'OBD' and 'String'
obd :: Prism' String OBD
obd = prism' conv mConv
  where
    conv x = printf "%04X" (x ^. re obdWord16)
    mConv s | [(x, "")] <- readHex s = x ^? obdWord16
            | otherwise = Nothing

-- | An OBD command requesting current vehicle data (mode 0x01).
data CurrentData = OBDEngineCoolantTemperature
                 | OBDEngineRPM
                 | OBDIntakeAirTemperature
                 | OBDMassAirFlowRate
                 | OBDSupported01PIDs
                 | OBDThrottlePosition
                 | OBDVehicleSpeed
                 deriving (Eq, Show)

-- | A prism between 'Word8' and 'CurrentData'.
currentData :: Prism' Word8 CurrentData
currentData = prism' conv mConv
  where
    conv OBDEngineCoolantTemperature = 0x05
    conv OBDEngineRPM = 0x0C
    conv OBDIntakeAirTemperature = 0x0F
    conv OBDMassAirFlowRate = 0x10
    conv OBDSupported01PIDs = 0x00
    conv OBDThrottlePosition = 0x11
    conv OBDVehicleSpeed = 0x0D

    mConv 0x05 = Just OBDEngineCoolantTemperature
    mConv 0x0C = Just OBDEngineRPM
    mConv 0x0F = Just OBDIntakeAirTemperature
    mConv 0x10 = Just OBDMassAirFlowRate
    mConv 0x00 = Just OBDSupported01PIDs
    mConv 0x11 = Just OBDThrottlePosition
    mConv 0x0D = Just OBDVehicleSpeed
    mConv _ = Nothing

-- | The protocol used by the ELM327 to connect to your car.
data Protocol = AutomaticProtocol
              | SAE_J1850_PWM
              | SAE_J1850_VPW
              | ISO_9141_2
              | ISO_14230_4_KWP
              | ISO_14230_4_KWP_Fast
              | ISO_15765_4_CAN_11_Bit_ID_500_KBaud
              | ISO_15765_4_CAN_29_Bit_ID_500_KBaud
              | ISO_15765_4_CAN_11_Bit_ID_250_KBaud
              | ISO_15765_4_CAN_29_Bit_ID_250_KBaud
              | SAE_J1939_CAN
              | USER1_CAN
              | USER2_CAN
              deriving (Eq, Show)

-- | A prism between 'String' and 'Protocol'.
protocol :: Prism' String Protocol
protocol = prism' conv mConv
  where
    conv AutomaticProtocol = "0"
    conv SAE_J1850_PWM = "1"
    conv SAE_J1850_VPW = "2"
    conv ISO_9141_2 = "3"
    conv ISO_14230_4_KWP = "4"
    conv ISO_14230_4_KWP_Fast = "5"
    conv ISO_15765_4_CAN_11_Bit_ID_500_KBaud = "6"
    conv ISO_15765_4_CAN_29_Bit_ID_500_KBaud = "7"
    conv ISO_15765_4_CAN_11_Bit_ID_250_KBaud = "8"
    conv ISO_15765_4_CAN_29_Bit_ID_250_KBaud = "9"
    conv SAE_J1939_CAN = "A"
    conv USER1_CAN = "B"
    conv USER2_CAN = "C"

    mConv "0" = Just AutomaticProtocol
    mConv "1" = Just SAE_J1850_PWM
    mConv "2" = Just SAE_J1850_VPW
    mConv "3" = Just ISO_9141_2
    mConv "4" = Just ISO_14230_4_KWP
    mConv "5" = Just ISO_14230_4_KWP_Fast
    mConv "6" = Just ISO_15765_4_CAN_11_Bit_ID_500_KBaud
    mConv "7" = Just ISO_15765_4_CAN_29_Bit_ID_500_KBaud
    mConv "8" = Just ISO_15765_4_CAN_11_Bit_ID_250_KBaud
    mConv "9" = Just ISO_15765_4_CAN_29_Bit_ID_250_KBaud
    mConv "A" = Just SAE_J1939_CAN
    mConv "B" = Just USER1_CAN
    mConv "C" = Just USER2_CAN
    mConv _ = Nothing
