-- | Module that simulates an ELM327 device.
module System.Hardware.ELM327.Simulator (
  -- * Initializing the simulator
  Simulator(..)
, defaultSimulator

  -- * Interacting with the simulator
, handle

  -- * Attributes of the simulator
, ConnectedProtocol(..)
, Echo(..)
, obdBus
, chosenProtocol
, connectedProtocol
, echo
, pin2Voltage
, versionID
) where

import Control.Lens (Lens', lens, (^.), (^?), (.~), re)
import Control.Monad.State (State, runState, get, put)
import Data.Char (toUpper)
import Text.Printf (printf)

import Numeric.Units.Dimensional.Prelude (ElectricPotential, volt, (*~), (/~))

import System.Hardware.ELM327.Commands (AT(..), Command(..), command,
                                        Protocol(..), protocol,
                                        obdMode, obdPID)
import System.Hardware.ELM327.Simulator.OBDBus (OBDBus)
import System.Hardware.ELM327.Utils.Hex (bytesToHex)
import qualified System.Hardware.ELM327.Simulator.OBDBus as OBDBus

-- | An actual simulator, with underlying bus, that holds all state.
data Simulator bus = Simulator { _obdBus :: bus
                               , _chosenProtocol :: Protocol
                               , _connectedProtocol :: ConnectedProtocol
                               , _echo :: Echo
                               , _pin2Voltage :: ElectricPotential Double
                               , _versionID :: String }

-- | The default simulator, initialized with a given protocol
defaultSimulator :: OBDBus bus => bus -> Simulator bus
defaultSimulator b = Simulator { _obdBus = b
                               , _chosenProtocol = AutomaticProtocol
                               , _connectedProtocol = NotConnected
                               , _echo = EchoOff
                               , _pin2Voltage = 12.3 *~ volt
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
    handle' (AT ATDescribeProtocolNumber) = reply <$> describeProtocolNumber
    handle' (AT ATEchoOff) = reply <$> echoOff
    handle' (AT ATReadVoltage) = reply <$> readVoltage
    handle' (AT ATResetAll) = reply <$> resetAll
    handle' (AT (ATSelectProtocol p)) = reply <$> selectProtocol p
    handle' (AT ATVersionID) = reply . (^. versionID) <$> get

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

-- | The voltage on pin 2 (most likely the battery voltage)
pin2Voltage :: Lens' (Simulator bus) (ElectricPotential Double)
pin2Voltage = lens _pin2Voltage $ \s x -> s { _pin2Voltage = x }

-- | The version ID of the ELM327.
versionID :: Lens' (Simulator bus) String
versionID = lens _versionID $ \s x -> s { _versionID = x }

-- | If the command is just a setting change, the ELM327 will reply with OK.
replyOK :: State (Simulator bus) String
replyOK = return "OK"

-- | Describe the protocol number
describeProtocolNumber :: State (Simulator bus) String
describeProtocolNumber = do
    cp <- (^. connectedProtocol) <$> get
    case cp of NotConnected -> return $ "A" ++ AutomaticProtocol ^. re protocol
               ManuallyChosen p -> return $ p ^. re protocol
               AutomaticallyChosen p -> return $ "A" ++ p ^. re protocol

-- | Turn echo off
echoOff :: State (Simulator bus) String
echoOff = (echo .~ EchoOff) <$> get >>= put >> replyOK

-- | Read the battery voltage
readVoltage :: State (Simulator bus) String
readVoltage = printf "%.1f" . (/~ volt) . (^. pin2Voltage) <$> get

-- | Fully reset the device
resetAll :: OBDBus bus => State (Simulator bus) String
resetAll = defaultSimulator . (^. obdBus) <$> get >>= put >> return ""

-- | Select a protocol
selectProtocol :: Protocol -> State (Simulator bus) String
selectProtocol p = (chosenProtocol .~ p) <$> get >>= put >> replyOK
