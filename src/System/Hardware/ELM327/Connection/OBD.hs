{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
-- | Send OBD requests to the connection and return a quantity.
module System.Hardware.ELM327.Connection.OBD where

import Control.Lens (Iso', re, (^.))
import Control.Monad.Trans.Either (EitherT(..), left)

import Numeric.Units.Dimensional.Prelude

import System.Hardware.ELM327.Commands (OBD(..), CurrentData(..))
import System.Hardware.ELM327.Connection (Con)
import System.Hardware.ELM327.Errors (OBDError(..), OBDDecodeError(..))
import qualified System.Hardware.ELM327.Connection as Connection
import qualified System.Hardware.ELM327.OBD.Conversion as C


-- | Type alias for a request.
type Request a = Con -> IO (Either OBDError a)

-- | Get the engine coolant temperature.
engineCoolantTemperature :: Request (CelsiusTemperature Double)
engineCoolantTemperature = getOneByte OBDEngineCoolantTemperature C.engineCoolantTemperature

-- | Get the engine RPM.
engineRPM :: Request (Frequency Double)
engineRPM = getTwoBytes OBDEngineRPM C.engineRPM

-- | Get the intake air temperature.
intakeAirTemperature :: Request (CelsiusTemperature Double)
intakeAirTemperature = getOneByte OBDIntakeAirTemperature C.intakeAirTemperature

-- | Get the intake manifold absolute pressure
intakeManifoldAbsolutePressure :: Request (Pressure Double)
intakeManifoldAbsolutePressure = getOneByte OBDIntakeManifoldAbsolutePressure C.intakeManifoldAbsolutePressure

-- | Get mass air flow rate.
massAirFlowRate :: Request (MassFlow Double)
massAirFlowRate = getTwoBytes OBDMassAirFlowRate C.massAirFlowRate

-- | Get throttle position.
throttlePosition :: Request Double
throttlePosition = getOneByte OBDThrottlePosition C.throttlePosition

-- | Get vehicle speed.
vehicleSpeed :: Request (Velocity Double)
vehicleSpeed = getOneByte OBDVehicleSpeed C.vehicleSpeed

-- | Get one byte of data and convert it.
getOneByte :: CurrentData -> Iso' a C.OneByte -> Request a
getOneByte cmd conv con = runEitherT $ (^. re conv) <$> (oneByte =<< obd)
    where obd = EitherT $ Connection.obd con (CurrentData cmd)
          oneByte [x] = return x
          oneByte _ = left $ OBDDecodeError NotEnoughBytesError

-- | Get two bytes of data and convert it.
getTwoBytes :: CurrentData -> Iso' a C.TwoBytes -> Request a
getTwoBytes cmd conv con = runEitherT $ (^. re conv) <$> (twoBytes =<< obd)
    where obd = EitherT $ Connection.obd con (CurrentData cmd)
          twoBytes [x, y] = return (x, y)
          twoBytes _ = left $ OBDDecodeError NotEnoughBytesError
