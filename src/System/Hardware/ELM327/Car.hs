{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
-- | A generic car data type
module System.Hardware.ELM327.Car (
  -- * Base car structure
  Car
, defaultCar

  -- * Monad transformer for cashing
, CarT(..)
, runCarT
, flushCache
, cache

  -- * Lenses for 'Car'
, engineCoolantTemperature
, engineFuelRate
, engineRPM
, intakeAirTemperature
, intakeManifoldAbsolutePressure
, massAirFlowRate
, throttlePosition
, vehicleSpeed

  -- * Internal structure
, CarState
, emptyState
) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar, modifyTVar)
import Control.Lens (Lens', lens, (^.), (.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Numeric.Units.Dimensional.Prelude

import System.Hardware.ELM327.Connection (ConT)
import qualified System.Hardware.ELM327.Connection.OBD as OBD

-- | A car that has some properties, see lenses documentation below.
data Car m = Car { _engineCoolantTemperature :: CarT m (ThermodynamicTemperature Double)
                 , _engineFuelRate :: CarT m (VolumeFlow Double)
                 , _engineRPM :: CarT m (Frequency Double)
                 , _intakeAirTemperature :: CarT m (ThermodynamicTemperature Double)
                 , _intakeManifoldAbsolutePressure :: CarT m (Pressure Double)
                 , _massAirFlowRate :: CarT m (MassFlow Double)
                 , _throttlePosition :: CarT m Double
                 , _vehicleSpeed :: CarT m (Velocity Double) }

-- | A monad transformer for 'Car' where requested data is cached until 'flushCache' is called.
newtype CarT m a = CarT { runCarT' :: ReaderT (TVar CarState) m a }
                 deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar CarState), MonadTrans)

-- | Run a 'CarT' with an initial empty state
runCarT :: MonadIO m => CarT m a -> m a
runCarT action = liftIO (newTVarIO emptyState) >>= runReaderT (runCarT' action)

-- | Flush the cache of a 'CarT'
flushCache :: MonadIO m => CarT m ()
flushCache = ask >>= liftIO . atomically . flip writeTVar emptyState

-- | Make an action cachable in 'CarT'
cache :: MonadIO m => (forall n . Lens' (Car n) (CarT n a)) -> CarT m a -> CarT m a
cache property action = do
    mv <- runCarT . (^. property) <$> (ask >>= liftIO . atomically . readTVar)
    v <- liftIO $ runMaybeT mv
    case v of
        Just x -> return x
        Nothing -> do
            v' <- action
            ask >>= liftIO . atomically . flip modifyTVar (property .~ lift (MaybeT . return $ Just v'))
            return v'

-- | The default car, that uses straight forward OBD commands to get
-- most of the data.
defaultCar :: MonadIO m => Car (ConT m)
defaultCar = Car { _engineCoolantTemperature = cache engineCoolantTemperature (lift OBD.engineCoolantTemperature)
                 , _engineFuelRate = cache engineFuelRate (lift OBD.engineFuelRate)
                 , _engineRPM = cache engineRPM (lift OBD.engineRPM)
                 , _intakeAirTemperature = cache intakeAirTemperature (lift OBD.intakeAirTemperature)
                 , _intakeManifoldAbsolutePressure = cache intakeManifoldAbsolutePressure (lift OBD.intakeManifoldAbsolutePressure)
                 , _massAirFlowRate = cache massAirFlowRate (lift OBD.massAirFlowRate)
                 , _throttlePosition = cache throttlePosition (lift OBD.throttlePosition)
                 , _vehicleSpeed = cache vehicleSpeed (lift OBD.vehicleSpeed) }

-- | The engine coolant temperature of the car.
engineCoolantTemperature :: Lens' (Car m) (CarT m (ThermodynamicTemperature Double))
engineCoolantTemperature = lens _engineCoolantTemperature $ \c x -> c { _engineCoolantTemperature = x }

-- | The engine fuel rate of the car.
engineFuelRate :: Lens' (Car m) (CarT m (VolumeFlow Double))
engineFuelRate = lens _engineFuelRate $ \c x -> c { _engineFuelRate = x }

-- | The engine RPM of the car.
engineRPM :: Lens' (Car m) (CarT m (Frequency Double))
engineRPM = lens _engineRPM $ \c x -> c { _engineRPM = x }

-- | The intake air temperature of the car.
intakeAirTemperature :: Lens' (Car m) (CarT m (ThermodynamicTemperature Double))
intakeAirTemperature = lens _intakeAirTemperature $ \c x -> c { _intakeAirTemperature = x }

-- | The intake manifold absolute pressure of the car.
intakeManifoldAbsolutePressure :: Lens' (Car m) (CarT m (Pressure Double))
intakeManifoldAbsolutePressure = lens _intakeManifoldAbsolutePressure $ \c x -> c { _intakeManifoldAbsolutePressure = x }

-- | The mass air flow rate of the car.
massAirFlowRate :: Lens' (Car m) (CarT m (MassFlow Double))
massAirFlowRate = lens _massAirFlowRate $ \c x -> c { _massAirFlowRate = x }

-- | The throttle position of the car.
throttlePosition :: Lens' (Car m) (CarT m Double)
throttlePosition = lens _throttlePosition $ \c x -> c { _throttlePosition = x }

-- | The throttle position of the car.
vehicleSpeed :: Lens' (Car m) (CarT m (Velocity Double))
vehicleSpeed = lens _vehicleSpeed $ \c x -> c { _vehicleSpeed = x }

-- | The pure state of the car, with possibly missing values.
type CarState = Car (MaybeT IO)

-- | The empty 'CarState'
emptyState :: CarState
emptyState = Car { _engineCoolantTemperature = lift (MaybeT $ return Nothing)
                 , _engineFuelRate = lift (MaybeT $ return Nothing)
                 , _engineRPM = lift (MaybeT $ return Nothing)
                 , _intakeAirTemperature = lift (MaybeT $ return Nothing)
                 , _intakeManifoldAbsolutePressure = lift (MaybeT $ return Nothing)
                 , _massAirFlowRate = lift (MaybeT $ return Nothing)
                 , _throttlePosition = lift (MaybeT $ return Nothing)
                 , _vehicleSpeed = lift (MaybeT $ return Nothing) }
