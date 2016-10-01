{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- | Helper functions to work with dimensions.
module System.Hardware.ELM327.Utils.Units where

import Data.Ratio ((%))

import Numeric.Units.Dimensional.Prelude (DFrequency,
                                          Metricality(NonMetric),
                                          Unit,
                                          hertz,
                                          mkUnitQ)
import Numeric.Units.Dimensional.UnitNames (atom)

-- | Declare rounds per minute
rpm :: Fractional a => Unit 'NonMetric DFrequency a
rpm = mkUnitQ (atom "m" "m" "min") (1 % 60) hertz
