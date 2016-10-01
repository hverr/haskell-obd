{-# LANGUAGE GADTs #-}
-- | Generic interface for an OBD bus that is supported by the ELM327 'Simulator'.
module System.Hardware.ELM327.Simulator.OBDBus where

import Data.Word (Word8)

import System.Hardware.ELM327.Commands (Protocol)

-- | Response to an OBD bus request.
--
-- Returns the new OBD bus simulator (with possibly modified state)
-- and an optional list of bytes. The simulated OBD bus should
-- return 'Nothing' if there is no data or the PID is not supported.
data Response bus where
    Response :: OBDBus bus => bus -> Maybe [Word8] -> Response bus

-- | An underlying bus that is supported by the ELM327 'Simulator'.
class OBDBus bus where
    protocol :: bus -> Protocol
    request :: bus -> [Word8] -> Response bus
