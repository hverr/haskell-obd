{-# LANGUAGE GADTs #-}
-- | Generic interface for an OBD bus that is supported by the ELM327 'Simulator'.
module System.Hardware.ELM327.Simulator.OBDBus where

import Control.Monad.State (State)
import Data.Word (Word8)

import System.Hardware.ELM327.Commands (Protocol)
import qualified System.Hardware.ELM327.Commands as Commands

-- | An underlying bus that is supported by the ELM327 'Simulator'.
class OBDBus bus where
    protocol :: bus -> Protocol
    request :: Commands.OBD -> State bus (Maybe [Word8])
