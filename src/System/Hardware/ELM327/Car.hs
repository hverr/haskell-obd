{-# LANGUAGE NoImplicitPrelude #-}
-- | A generic car data type
module System.Hardware.ELM327.Car where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)

import Numeric.Units.Dimensional.Prelude

import System.Hardware.ELM327.Connection (Con)
import System.Hardware.ELM327.Errors (OBDError)
import qualified System.Hardware.ELM327.Connection.OBD as OBD

-- | Some info about the car.
type I m a = m (Either OBDError a)

-- | A monad transform for information about the car.
infoT :: I m a -> EitherT OBDError m a
infoT = EitherT

-- | Run the monad transform for inforamtion about the ar.
runInfoT :: EitherT OBDError m a -> I m a
runInfoT = runEitherT

-- | A car that has some properties.
data Car m = Car { engineCoolantTemperature :: I m (ThermodynamicTemperature Double)
                 , engineFuelRate :: I m (VolumeFlow Double)
                 , engineRPM :: I m (Frequency Double)
                 , intakeAirTemperature :: I m (ThermodynamicTemperature Double)
                 , intakeManifoldAbsolutePressure :: I m (Pressure Double)
                 , massAirFlowRate :: I m (MassFlow Double)
                 , throttlePosition :: I m Double
                 , vehicleSpeed :: I m (Velocity Double) }

-- | The default car, that uses straight forward OBD commands to get
-- most of the data.
defaultCar :: MonadIO m => Con -> Car m
defaultCar c = Car { engineCoolantTemperature = liftIO $ OBD.engineCoolantTemperature c
                   , engineFuelRate = liftIO $ OBD.engineFuelRate c
                   , engineRPM = liftIO $ OBD.engineRPM c
                   , intakeAirTemperature = liftIO $ OBD.intakeAirTemperature c
                   , intakeManifoldAbsolutePressure = liftIO $ OBD.intakeManifoldAbsolutePressure c
                   , massAirFlowRate = liftIO $ OBD.massAirFlowRate c
                   , throttlePosition = liftIO $ OBD.throttlePosition c
                   , vehicleSpeed = liftIO $ OBD.vehicleSpeed c }
