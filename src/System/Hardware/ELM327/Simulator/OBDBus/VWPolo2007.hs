{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Simulated bus of a Volkswagen Polo 2007.
module System.Hardware.ELM327.Simulator.OBDBus.VWPolo2007 where

import Control.Lens (Lens', lens, (^.))
import Control.Monad.State (get)
import Data.Word (Word8)

import Numeric.Units.Dimensional.Prelude

import System.Hardware.ELM327.Commands (CurrentData(..), OBD(..), Protocol(ISO_14230_4_KWP))
import System.Hardware.ELM327.Simulator.OBDBus (OBDBus, protocol, request)

import qualified System.Hardware.ELM327.OBD.Conversion as C

-- | State of the simulated bus.
data VWPolo2007Bus = Bus { _engineCoolantTemperature :: CelsiusTemperature Double
                         , _engineRPM :: Frequency Double
                         , _intakeAirTemperature :: CelsiusTemperature Double
                         , _massAirFlowRate :: MassFlow Double
                         , _throttlePosition :: Double
                         , _vehicleSpeed :: Velocity Double }
                         deriving (Show)

-- | Supported mode 0x01 PIDs of the bus
supported01PIDs :: [Word8]
supported01PIDs = [0xBE, 0x3E, 0xB8, 0x11]

-- | The 'VWPolo2007Bus' is an instance of 'OBDBus'.
instance OBDBus VWPolo2007Bus where
    protocol _ = ISO_14230_4_KWP
    request (CurrentData x) = reqCD x
      where
        reqCD OBDEngineCoolantTemperature = oneByte engineCoolantTemperature C.engineCoolantTemperature <$> get
        reqCD OBDEngineRPM = twoBytes engineRPM C.engineRPM <$> get
        reqCD OBDIntakeAirTemperature = oneByte intakeAirTemperature C.intakeAirTemperature <$> get
        reqCD OBDMassAirFlowRate = twoBytes massAirFlowRate C.massAirFlowRate <$> get
        reqCD OBDSupported01PIDs = return $ Just supported01PIDs
        reqCD OBDThrottlePosition = oneByte throttlePosition C.throttlePosition <$> get
        reqCD OBDVehicleSpeed = oneByte vehicleSpeed C.vehicleSpeed <$> get

        oneByte getter isomorph = Just . C.oneByte . (^. (getter . isomorph))
        twoBytes getter isomorph = Just . C.twoBytes . (^. (getter . isomorph))

-- | The bus of a stopped car
stoppedCarBus :: VWPolo2007Bus
stoppedCarBus = Bus { _engineCoolantTemperature = 20 *~ degreeCelsius
                    , _engineRPM = _0
                    , _intakeAirTemperature = 20 *~ degreeCelsius
                    , _massAirFlowRate = _0
                    , _throttlePosition = 0
                    , _vehicleSpeed = _0 }

-- | The engine coolant temperature.
engineCoolantTemperature :: Lens' VWPolo2007Bus (CelsiusTemperature Double)
engineCoolantTemperature = lens _engineCoolantTemperature $ \b x -> b { _engineCoolantTemperature = x }

-- | The engine RPM.
engineRPM :: Lens' VWPolo2007Bus (Frequency Double)
engineRPM = lens _engineRPM $ \b x -> b { _engineRPM = x }

-- | The intake air temperature.
intakeAirTemperature :: Lens' VWPolo2007Bus (CelsiusTemperature Double)
intakeAirTemperature = lens _intakeAirTemperature $ \b x -> b { _intakeAirTemperature = x }

-- | The mass air flow rate.
massAirFlowRate :: Lens' VWPolo2007Bus (MassFlow Double)
massAirFlowRate = lens _massAirFlowRate $ \b x -> b { _massAirFlowRate = x }

-- | The mass air flow rate.
throttlePosition :: Lens' VWPolo2007Bus Double
throttlePosition = lens _throttlePosition $ \b x -> b { _throttlePosition = x }

-- | The speed of the vehicle.
vehicleSpeed :: Lens' VWPolo2007Bus (Velocity Double)
vehicleSpeed = lens _vehicleSpeed $ \b x -> b { _vehicleSpeed = x }
