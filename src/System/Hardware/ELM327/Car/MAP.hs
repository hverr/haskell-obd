{-# LANGUAGE NoImplicitPrelude #-}
-- | A car that uses the intake manifold absolute pressure (MAP) to
-- calculate fuel consumption.
module System.Hardware.ELM327.Car.MAP (
  MAPProperties(..)
, defaultProperties
, mapCar
) where

import Control.Lens ((^. ), (.~))
import Control.Monad.IO.Class (MonadIO)

import Numeric.Units.Dimensional.Prelude

import System.Hardware.ELM327.Connection (ConT)
import System.Hardware.ELM327.Car (CarT, Car, defaultCar)
import qualified System.Hardware.ELM327.Car as Car


-- | Some fixed values needed to implement a MAP car.
data MAPProperties = MAPProperties { volumetricEfficiency :: Dimensionless Double
                                   , engineDisplacement :: Volume Double
                                   , airFuelRatio :: Dimensionless Double
                                   , fuelMassDensity :: MassDensity Double }

-- | The default MAP properties
defaultProperties :: MAPProperties
defaultProperties = MAPProperties { volumetricEfficiency = 0.75 *~ one                 -- consumer engine
                                  , engineDisplacement = 1198 *~ cubic (centi meter)   -- 1.2l
                                  , airFuelRatio = 14.7 *~ one                         -- chemically ideal
                                  , fuelMassDensity = 750 *~ (kilo gram / cubic meter) -- EUROSUPER 95
                                  }

-- | Create a car that uses the intake manifold absolute pressure (MAP)
-- to calculate fuel consumption.
mapCar :: MonadIO m => MAPProperties -> Car (ConT m)
mapCar p = (\c -> (Car.massAirFlowRate .~ massAirFlowRate' p c) c) .
           (\c -> (Car.engineFuelRate  .~ engineFuelRate'  p c) c) $
           defaultCar

-- | Calculate MAF from RPM, MAP and IAT
--
-- See https://web.archive.org/web/20160323073219/http://www.lightner.net/obd2guru/IMAP_AFcalc.html
massAirFlowRate' :: Monad m  => MAPProperties -> Car m -> CarT m (MassFlow Double)
massAirFlowRate' p c = do
    rpm  <- c ^. Car.engineRPM
    map' <- c ^. Car.intakeManifoldAbsolutePressure
    iat  <- c ^. Car.intakeAirTemperature
    let imap = rpm * map' / iat / (2 *~ one)
    let voleff = volumetricEfficiency p
    let displ = engineDisplacement p
    let mm = 28.97 *~ (gram / mole)
    let r = 8.314 *~ (joule / kelvin / mole)
    return $ imap * voleff * displ * mm / r

-- | Calculate the engine fuel rate from MAF
engineFuelRate' :: Monad m => MAPProperties -> Car m -> CarT m (VolumeFlow Double)
engineFuelRate' p c = do
    maf <- c ^. Car.massAirFlowRate
    return $ maf / airFuelRatio p / fuelMassDensity p
