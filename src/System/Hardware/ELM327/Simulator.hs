-- | Module that simulates an ELM327 device.
module System.Hardware.ELM327.Simulator where

import Control.Lens (Lens', lens)

import System.Hardware.ELM327.Commands (Protocol(..))

-- | An actual simulator, holds all state.
data Simulator = Simulator { _actualProtocol :: Protocol
                           , _chosenProtocol :: Protocol
                           , _connectedProtocol :: ConnectedProtocol
                           , _echo :: Echo
                           , _versionID :: String }

-- | The default simulator, initialized with a given protocol
defaultSimulator :: Protocol -> Simulator
defaultSimulator p = Simulator { _actualProtocol = p
                               , _chosenProtocol = AutomaticProtocol
                               , _connectedProtocol = NotConnected
                               , _echo = EchoOff
                               , _versionID = "ELM327 v2.1" }

-- | The protocol the ELM327 simulator is setup to use.
data ConnectedProtocol = NotConnected
                    | ManuallyChosen Protocol
                    | AutomaticallyChosen Protocol
                    deriving (Eq, Show)

-- | The echo settings of the ELM327.
data Echo = EchoOn | EchoOff deriving (Eq, Show)

-- | The actual underlying protocol of the simulator.
actualProtocol :: Lens' Simulator Protocol
actualProtocol = lens _actualProtocol $ \s x -> s { _actualProtocol = x }

-- | The protocol the simulator should try to connect with.
chosenProtocol :: Lens' Simulator Protocol
chosenProtocol = lens _chosenProtocol $ \s x -> s { _chosenProtocol = x }

-- | The protocol the ELM327 simulator is connected with.
connectedProtocol :: Lens' Simulator ConnectedProtocol
connectedProtocol = lens _connectedProtocol $ \s x -> s { _connectedProtocol = x }

-- | The echo settings of the ELM327.
echo :: Lens' Simulator Echo
echo = lens _echo $ \s x -> s { _echo = x }

-- | The version ID of the ELM327.
versionID :: Lens' Simulator String
versionID = lens _versionID $ \s x -> s { _versionID = x }
