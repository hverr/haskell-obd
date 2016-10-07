{-# LANGUAGE NoImplicitPrelude #-}
-- | A generic car data type
module System.Hardware.ELM327.Car where

import Numeric.Units.Dimensional.Prelude

data Car m = Car { engineCoolantTemperature :: m (CelsiusTemperature Double)
                 , engineRPM :: m (Frequency Double)
                 , intakeAirTemperature :: m (CelsiusTemperature Double)
                 , intakeManifoldAbsolutePressure :: m (Pressure Double)
                 , massAirFlowRate :: m (MassFlow Double)
                 , throttlePosition :: m Double
                 , vehicleSpeed :: m (Velocity Double) }
