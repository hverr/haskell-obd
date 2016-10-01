-- | Simulated bus of a Volkswagen Polo 2007.
module System.Hardware.ELM327.Simulator.OBDBus.VWPolo2007 where

import System.Hardware.ELM327.Commands (Protocol(SAE_J1850_PWM))
import System.Hardware.ELM327.Simulator.OBDBus (OBDBus, Response(..), protocol, request)

-- | State of the simulated bus.
data VWPolo2007Bus

instance OBDBus VWPolo2007Bus where
    protocol _ = SAE_J1850_PWM
    request bus _ = Response bus Nothing
