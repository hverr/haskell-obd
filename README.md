haskell-obd
===========

[![Build Status](https://travis-ci.org/hverr/haskell-obd.svg?branch=master)](https://travis-ci.org/hverr/haskell-obd)
[![Hackage](https://img.shields.io/hackage/v/obd.svg?maxAge=2592000)](https://hackage.haskell.org/package/obd)
[![Stackage Nightly](http://stackage.org/package/obd/badge/nightly)](http://stackage.org/nightly/package/obd)
[![Stackage LTS](http://stackage.org/package/obd/badge/lts)](http://stackage.org/lts/package/obd)

Haskell library to communicate with OBD-II over ELM327, with

  - type-safe quantities with dimensions
  - extensible simulator
  - example terminal as a demo

Usage
-----

Look in [`app/Terminal.hs`](app/Terminal.hs) for a very basic example.

The interface to the engine is shown below.

```haskell
module Main where

import System.Hardware.ELM327 (connect)
import System.Hardware.ELM327.Commands (AT(..), Protocol(..))
import System.Hardware.ELM327.Connection (Con, close, at)
import System.Hardware.ELM327.Car (Car(..), defaultCar infoT)

main :: IO ()
main = do
    con <- connect "/dev/ttyUSB0"
    _ <- at con (ATSelectProtocol AutomaticProtocol)
    let car = defaultCar con

    printI "ECT"       $ engineCoolantTemperature       car
    printI "Fuel Rate" $ engineFuelRate                 car
    printI "RPM"       $ engineRPM                      car
    printI "Air T."    $ intakeAirTemperature           car
    printI "MAP"       $ intakeManifoldAbsolutePressure car
    printI "MAF"       $ massAirFlowRate                car
    printI "Throttle"  $ throttlePosition               car
    printI "Speed"     $ vehicleSpeed                   car

    close con
  where
    printI key value = do
        v <- show <$> value
        putStrLn $ key ++ ": " ++ v

```
