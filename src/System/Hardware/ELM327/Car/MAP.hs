-- | A car that uses the intake manifold absolute pressure (MAP) to
-- calculate fuel consumption.
module System.Hardware.ELM327.Car.MAP where

import Control.Monad.IO.Class (MonadIO)

import System.Hardware.ELM327.Connection (Con)
import System.Hardware.ELM327.Car (Car(..), defaultCar)

-- | Create a car that uses the intake manifold absolute pressure (MAP)
-- to calculate fuel consumption.
mapCar :: MonadIO m => Con -> Car m
mapCar c = (defaultCar c) { engineFuelRate = undefined
                          , massAirFlowRate = undefined }
