-- | Module that simulates an ELM327 device.
module System.Hardware.ELM327.Simulator where

import Control.Lens (Lens', lens, (^.), (^?), (.~))
import Control.Monad.State (State, runState, get, put)
import Data.Char (toUpper)


import System.Hardware.ELM327.Commands (Command(..), Protocol(..), command, obdMode, obdPID)
import System.Hardware.ELM327.Simulator.OBDBus (OBDBus)
import System.Hardware.ELM327.Utils.Hex (bytesToHex)
import qualified System.Hardware.ELM327.Simulator.OBDBus as OBDBus

-- | An actual simulator, with underlying bus, that holds all state.
data Simulator bus = Simulator { _obdBus :: bus
                               , _chosenProtocol :: Protocol
                               , _connectedProtocol :: ConnectedProtocol
                               , _echo :: Echo
                               , _versionID :: String }

-- | The default simulator, initialized with a given protocol
defaultSimulator :: OBDBus bus => bus -> Simulator bus
defaultSimulator b = Simulator { _obdBus = b
                               , _chosenProtocol = AutomaticProtocol
                               , _connectedProtocol = NotConnected
                               , _echo = EchoOff
                               , _versionID = "ELM327 v2.1" }

-- | Make the simulator handle a command.
handle :: OBDBus bus => String -> State (Simulator bus) String
handle cmd = maybe (return "?") handle' $ sanitize cmd
  where
    sanitize = (^? command) . map toUpper . filter (/= ' ')

    handle' :: OBDBus bus => Command -> State (Simulator bus) String
    handle' (OBD x) = do
        bus <- (^. obdBus) <$> get
        let (resp, bus') = runState (OBDBus.request x) bus
        (obdBus .~ bus') <$> get >>= put
        case resp of
            Nothing -> return $ reply "NO DATA"
            Just v -> return . reply . bytesToHex " " $ [obdMode x + 0x40, obdPID x] ++ v
    handle' _ = return "OK"

    reply = (++ "\r>")

-- | The protocol the ELM327 simulator is setup to use.
data ConnectedProtocol = NotConnected
                       | ManuallyChosen Protocol
                       | AutomaticallyChosen Protocol
                       deriving (Eq, Show)

-- | The echo settings of the ELM327.
data Echo = EchoOn | EchoOff deriving (Eq, Show)

-- | The actual underlying simulated bus of the simulator.
obdBus :: OBDBus bus => Lens' (Simulator bus) bus
obdBus = lens _obdBus $ \s x -> s { _obdBus = x }

-- | The protocol the simulator should try to connect with.
chosenProtocol :: Lens' (Simulator bus) Protocol
chosenProtocol = lens _chosenProtocol $ \s x -> s { _chosenProtocol = x }

-- | The protocol the ELM327 simulator is connected with.
connectedProtocol :: Lens' (Simulator bus) ConnectedProtocol
connectedProtocol = lens _connectedProtocol $ \s x -> s { _connectedProtocol = x }

-- | The echo settings of the ELM327.
echo :: Lens' (Simulator bus) Echo
echo = lens _echo $ \s x -> s { _echo = x }

-- | The version ID of the ELM327.
versionID :: Lens' (Simulator bus) String
versionID = lens _versionID $ \s x -> s { _versionID = x }
