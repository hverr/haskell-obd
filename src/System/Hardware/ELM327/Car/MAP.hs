{-# LANGUAGE NoImplicitPrelude #-}
-- | A car that uses the intake manifold absolute pressure (MAP) to
-- calculate fuel consumption.
module System.Hardware.ELM327.Car.MAP (
  MAPProperties
, defaultProperties
, mapCar
) where

import Control.Monad.IO.Class (MonadIO)

import Numeric.Units.Dimensional.Prelude

import System.Hardware.ELM327.Connection (Con)
import System.Hardware.ELM327.Car (Car(..), I, infoT, runInfoT, defaultCar)


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
mapCar :: MonadIO m => Con -> MAPProperties ->Car m
mapCar con p = let c = defaultCar con in
               c { engineFuelRate = engineFuelRate' p c
                 , massAirFlowRate = massAirFlowRate' p c}

-- | Calculate MAF from RPM, MAP and IAT
--
-- See https://web.archive.org/web/20160323073219/http://www.lightner.net/obd2guru/IMAP_AFcalc.html
massAirFlowRate' :: Monad m  => MAPProperties -> Car m -> I m (MassFlow Double)
massAirFlowRate' p c = runInfoT $ do
    rpm  <- infoT $ engineRPM c
    map' <- infoT $ intakeManifoldAbsolutePressure c
    iat  <- infoT $ intakeAirTemperature c
    let imap = rpm * map' / iat / (2 *~ one)
    let voleff = volumetricEfficiency p
    let displ = engineDisplacement p
    let mm = 28.97 *~ (gram / mole)
    let r = 8.314 *~ (joule / kelvin / mole)
    return $ imap * voleff * displ * mm / r

-- | Calculate the engine fuel rate from MAF
engineFuelRate' :: Monad m => MAPProperties -> Car m -> I m (VolumeFlow Double)
engineFuelRate' p c = runInfoT $ do
    maf <- infoT $ massAirFlowRate' p c
    return $ maf / airFuelRatio p / fuelMassDensity p
