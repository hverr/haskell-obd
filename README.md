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

Look in [`app/Terminal.hs`](app/Terminal.hs) for a very basic example for low-level communication.

The interface to the engine is shown below and in [`app/Example.hs`](app/Example.hs).

```haskell
module Main where

import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import System.Hardware.ELM327 (connect)
import System.Hardware.ELM327.Commands (AT(..), Protocol(..))
import System.Hardware.ELM327.Connection (Con, ConT, ConError, withCon, close', at)
import System.Hardware.ELM327.Car (Car, runCarT, defaultCar)
import qualified System.Hardware.ELM327.Car as Car

main :: IO ()
main = do
    r <- bracket (either (fail . show) return =<< connect "/dev/ttyUSB0")
                 (\c -> putStrLn "Closing connection..." >> close' c)
                 query
    either (fail . show) return r

query :: Con -> IO (Either ConError ())
query con = withCon con $ runCarT $ do
    _ <- lift $ at (ATSelectProtocol AutomaticProtocol)
    printI "ECT"       $ car ^. Car.engineCoolantTemperature
    printI "Fuel Rate" $ car ^. Car.engineFuelRate
    printI "RPM"       $ car ^. Car.engineRPM
    printI "Air T."    $ car ^. Car.intakeAirTemperature
    printI "MAP"       $ car ^. Car.intakeManifoldAbsolutePressure
    printI "MAF"       $ car ^. Car.massAirFlowRate
    printI "Throttle"  $ car ^. Car.throttlePosition
    printI "Speed"     $ car ^. Car.vehicleSpeed
  where
    car :: Car (ConT IO)
    car = defaultCar
    printI key value = do
        v <- show <$> value
        liftIO $ putStrLn $ key ++ ": " ++ v
```
