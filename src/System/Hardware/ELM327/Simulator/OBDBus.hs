{-# LANGUAGE GADTs #-}
-- | Generic interface for an OBD bus that is supported by the ELM327 'Simulator'.
module System.Hardware.ELM327.Simulator.OBDBus where

import Control.Monad.State (State)
import Data.Word (Word8)

import System.Hardware.ELM327.Commands (Protocol)

-- | An underlying bus that is supported by the ELM327 'Simulator'.
class OBDBus bus where
    protocol :: bus -> Protocol
    request :: OBDBus bus => [Word8] -> State bus (Maybe [Word8])
