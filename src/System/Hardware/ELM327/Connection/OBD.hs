{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
-- | Send OBD requests to the connection and return a quantity.
module System.Hardware.ELM327.Connection.OBD where

import Control.Lens (Iso', re, (^.))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)

import Numeric.Units.Dimensional.Prelude

import System.Hardware.ELM327.Commands (OBD(..), CurrentData(..))
import System.Hardware.ELM327.Connection (ConT, ConError(..))
import System.Hardware.ELM327.Errors (OBDError(..), OBDDecodeError(..))
import qualified System.Hardware.ELM327.Connection as Connection
import qualified System.Hardware.ELM327.OBD.Conversion as C

-- | Get the engine coolant temperature.
engineCoolantTemperature :: MonadIO m => ConT m (ThermodynamicTemperature Double)
engineCoolantTemperature = getOneByte OBDEngineCoolantTemperature C.engineCoolantTemperature

-- | Get the engine fuel rate.
engineFuelRate :: MonadIO m => ConT m (VolumeFlow Double)
engineFuelRate = getTwoBytes OBDEngineFuelRate C.engineFuelRate

-- | Get the engine RPM.
engineRPM :: MonadIO m => ConT m (Frequency Double)
engineRPM = getTwoBytes OBDEngineRPM C.engineRPM

-- | Get the intake air temperature.
intakeAirTemperature :: MonadIO m => ConT m (ThermodynamicTemperature Double)
intakeAirTemperature = getOneByte OBDIntakeAirTemperature C.intakeAirTemperature

-- | Get the intake manifold absolute pressure
intakeManifoldAbsolutePressure :: MonadIO m => ConT m (Pressure Double)
intakeManifoldAbsolutePressure = getOneByte OBDIntakeManifoldAbsolutePressure C.intakeManifoldAbsolutePressure

-- | Get mass air flow rate.
massAirFlowRate :: MonadIO m => ConT m (MassFlow Double)
massAirFlowRate = getTwoBytes OBDMassAirFlowRate C.massAirFlowRate

-- | Get throttle position.
throttlePosition :: MonadIO m => ConT m Double
throttlePosition = getOneByte OBDThrottlePosition C.throttlePosition

-- | Get vehicle speed.
vehicleSpeed :: MonadIO m => ConT m (Velocity Double)
vehicleSpeed = getOneByte OBDVehicleSpeed C.vehicleSpeed

-- | Get one byte of data and convert it.
getOneByte :: MonadIO m => CurrentData -> Iso' a C.OneByte -> ConT m a
getOneByte cmd conv = (^. re conv) <$> (oneByte =<< obd)
    where obd = Connection.obd (CurrentData cmd)
          oneByte [x] = return x
          oneByte _ = throwError $ ConOBDError (OBDDecodeError NotEnoughBytesError)

-- | Get two bytes of data and convert it.
getTwoBytes :: MonadIO m => CurrentData -> Iso' a C.TwoBytes -> ConT m a
getTwoBytes cmd conv = (^. re conv) <$> (twoBytes =<< obd)
    where obd = Connection.obd (CurrentData cmd)
          twoBytes [x, y] = return (x, y)
          twoBytes _ = throwError $ ConOBDError (OBDDecodeError NotEnoughBytesError)
