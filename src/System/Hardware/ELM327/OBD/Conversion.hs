{-# LANGUAGE NoImplicitPrelude #-}
-- | This module contains structures to convert measurements and
-- values from and to OBD bytes.
module System.Hardware.ELM327.OBD.Conversion where

import qualified Prelude as P

import Control.Lens (Iso', iso, re, (^.))
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Word (Word8)

import Numeric.Units.Dimensional.Prelude

import System.Hardware.ELM327.Utils.Units (rpm)

-- | Type alias for a tuple of two bytes.
type TwoBytes = (Word8, Word8)

-- | Convert 'TwoBytes' to a byte array
twoBytes :: TwoBytes -> [Word8]
twoBytes (a, b) = [a, b]

-- | Type alias for one byte
type OneByte = Word8

-- | Convert 'OneByte' to a byte array
oneByte :: OneByte -> [Word8]
oneByte x = [x]

-- | Convert engine coolant temperature.
engineCoolantTemperature :: Iso' (CelsiusTemperature Double) OneByte
engineCoolantTemperature = iso conv mConv
  where conv = (^. doubleOneByte) . (P.+ 40) . (/~ degreeCelsius)
        mConv = (*~ degreeCelsius) . (P.- 40) . (^. re doubleOneByte)

-- | Convert engine fuel rate.
engineFuelRate :: Iso' (VolumeFlow Double) TwoBytes
engineFuelRate = iso conv mConv
  where conv = (^. doubleTwoBytes) . (P.* 20) . (/~ (liter / hour))
        mConv = (*~ (liter / hour)) . (P./ 20) . (^. re doubleTwoBytes)

-- | Convert engine RPM.
engineRPM :: Iso' (Frequency Double) TwoBytes
engineRPM = iso conv mConv
  where conv = (^. doubleTwoBytes) . (P.* 4) . (/~ rpm)
        mConv = (*~ rpm) . (P./ 4) . (^. re doubleTwoBytes)

-- | Convert intake air temperature.
intakeAirTemperature :: Iso' (CelsiusTemperature Double) OneByte
intakeAirTemperature = iso conv mConv
  where conv = (^. doubleOneByte) . (P.+ 40) . (/~ degreeCelsius)
        mConv = (*~ degreeCelsius) . (P.- 40) . (^. re doubleOneByte)

-- | Convert intake manifold absolute pressure.
intakeManifoldAbsolutePressure :: Iso' (Pressure Double) OneByte
intakeManifoldAbsolutePressure = iso conv mConv
  where conv = (^. doubleOneByte) . (/~ kilo pascal)
        mConv = (*~ kilo pascal) . (^. re doubleOneByte)

-- | Convert mass air flow rate.
massAirFlowRate :: Iso' (MassFlow Double) TwoBytes
massAirFlowRate = iso conv mConv
  where conv = (^. doubleTwoBytes) . (P.* 100) . (/~ (gram / second))
        mConv = (*~ (gram / second)) . (P./ 100) . (^. re doubleTwoBytes)

-- | Convert throttle position
throttlePosition :: Iso' Double OneByte
throttlePosition = iso conv mConv
  where conv = (^. doubleOneByte) . (P.* 255)
        mConv = (P./ 255) . (^. re doubleOneByte)

-- | Convert vehicle speed
vehicleSpeed :: Iso' (Velocity Double) OneByte
vehicleSpeed = iso conv mConv
  where conv = (^. doubleOneByte) . (/~ (kilo meter / second))
        mConv = (*~ (kilo meter / second)) . (^. re doubleOneByte)

-- | Convert between 'Double' and two bytes.
doubleTwoBytes :: Iso' Double TwoBytes
doubleTwoBytes = iso conv mConv
  where conv = (^. integerTwoBytes) . round
        mConv = realToFrac . (^. re integerTwoBytes)

-- | Convert between 'Double' and one byte.
doubleOneByte :: Iso' Double OneByte
doubleOneByte = iso conv mConv
  where conv = (^. integerOneByte) . round
        mConv = realToFrac

-- | Convert between 'Int' and two bytes.
integerTwoBytes :: Iso' Int TwoBytes
integerTwoBytes = iso conv mConv
  where conv x | x <= 0x0000 = (0x00, 0x00)
               | x >= 0xFFFF = (0xFF, 0xFF)
               | otherwise = (fromIntegral $ shiftR x 8 .&. 0xFF, fromIntegral $ x .&. 0xFF)
        mConv (a, b) = shiftL (fromIntegral a) 8 P.+ fromIntegral b

-- | Conver between 'Int' and one byte.
integerOneByte :: Iso' Int OneByte
integerOneByte = iso conv mConv
  where conv x | x <= 0x00 = 0x00
               | x >= 0xFF = 0xFF
               | otherwise = fromIntegral x
        mConv = fromIntegral
