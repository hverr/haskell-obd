-- | Module that simulates an ELM327 device.
module System.Hardware.ELM327.Simulator (
  -- * Initializing the simulator
  Simulator(..)
, defaultSimulator

  -- * Interacting with the simulator
, connect
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

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TMVar, atomically, newTMVarIO, takeTMVar, putTMVar)
import Control.Concurrent.STM.TMChan (TMChan, newTMChanIO, closeTMChan, readTMChan, writeTMChan)
import Control.Lens (Lens', lens, (^.), (^?), (.~), re)
import Control.Monad (void)
import Control.Monad.State (StateT, runState, runStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString)
import Data.Char (isHexDigit, toUpper)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as Char8

import Numeric.Units.Dimensional.Prelude (ElectricPotential, volt, (*~), (/~))

import System.IO.Streams (makeInputStream, makeOutputStream)

import System.Hardware.ELM327.Commands (AT(..), Command(..), command,
                                        Protocol(..), protocol,
                                        obdMode, obdPID)
import System.Hardware.ELM327.Connection (Con(..))
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
                               deriving (Show)

-- | The default simulator, initialized with a given protocol
defaultSimulator :: OBDBus bus => bus -> Simulator bus
defaultSimulator b = Simulator { _obdBus = b
                               , _chosenProtocol = AutomaticProtocol
                               , _connectedProtocol = NotConnected
                               , _echo = EchoOff
                               , _pin2Voltage = 12.3 *~ volt
                               , _versionID = "ELM327 v2.1" }

-- | Private data declaration that represents a connection to the simulator.
data SimCon bus = SimCon { _conInputBuffer :: TMChan ByteString
                         , _conOutputBuffer :: TMChan ByteString
                         , _unhandledInput :: TMVar ByteString }

-- | Initialize a 'SimulatorCon'
simCon :: IO (SimCon bus)
simCon = do
    ib <- newTMChanIO
    ob <- newTMChanIO
    ui <- newTMVarIO ""
    return SimCon { _conInputBuffer = ib
             , _conOutputBuffer = ob
             , _unhandledInput = ui }

-- | Connect to the simulator.
connect :: OBDBus bus => Simulator bus -> IO Con
connect s = do
    c <- simCon
    is <- makeInputStream (produce c)
    os <- makeOutputStream (consume c)
    _ <- forkIO . void $ runStateT (runSimulator c) s
    return $ Con is os (const $ return ())
  where
    produce = atomically . readTMChan . _conOutputBuffer
    consume c Nothing = atomically $ closeTMChan (_conInputBuffer c)
    consume c (Just x) = atomically $ writeTMChan (_conInputBuffer c) x

    runSimulator :: OBDBus bus => SimCon bus -> StateT (Simulator bus) IO ()
    runSimulator c = do
        next <- liftIO $ recv c
        case next of
            Nothing -> liftIO . atomically $ closeTMChan (_conOutputBuffer c)
            Just cmd -> do response <- handle (Char8.unpack cmd)
                           liftIO . atomically $ writeTMChan (_conOutputBuffer c) (Char8.pack response)
                           runSimulator c

    recv c = atomically $ recv' c =<< takeTMVar (_unhandledInput c)
    recv' c bs = do
        let (pref, suf) = Char8.break (== '\r') bs
        if Char8.null suf then do next <- readTMChan (_conInputBuffer c)
                                  case next of Nothing -> return Nothing
                                               Just x -> recv' c (Char8.append bs x)
                          else do putTMVar (_unhandledInput c) (Char8.tail suf)
                                  return $ Just pref

-- | Private data declaration that represents a parsed command (used in 'handle')
data ParsedCommand = InvalidCommand
                   | KnownCommand Command
                   | UnknownOBD String

-- | Parse a command from a serial line
parsedCommand :: String -> ParsedCommand
parsedCommand = parsedCommand' . map toUpper . filter (/= ' ')
  where
    parsedCommand' c@('A':'T':_) = maybe InvalidCommand KnownCommand (c ^? command)
    parsedCommand' c | all isHexDigit c = maybe (UnknownOBD c) KnownCommand (c ^? command)
                     | otherwise        = InvalidCommand

-- | Make the simulator handle a command.
handle :: (Monad m, OBDBus bus) => String -> StateT (Simulator bus) m String
handle cmd = handle' $ parsedCommand cmd
  where
    handle' :: (Monad m, OBDBus bus) => ParsedCommand -> StateT (Simulator bus) m String
    handle' InvalidCommand = return "?"
    handle' (UnknownOBD _) = reply . either id (++ "NO DATA") <$> connectToBus
    handle' (KnownCommand x) = handle'' x
    handle'' (OBD x) = do
        connectStatus <- connectToBus
        case connectStatus of
            Left errMsg -> return $ reply errMsg
            Right conMsg -> do
                bus <- (^. obdBus) <$> get
                let (resp, bus') = runState (OBDBus.request x) bus
                (obdBus .~ bus') <$> get >>= put
                case resp of
                    Nothing -> return . reply $ conMsg ++ "NO DATA"
                    Just v -> return . reply . (conMsg ++) . bytesToHex " " $ [obdMode x + 0x40, obdPID x] ++ v
    handle'' (AT ATDescribeProtocolNumber) = reply <$> describeProtocolNumber
    handle'' (AT ATEchoOff) = reply <$> echoOff
    handle'' (AT ATReadVoltage) = reply <$> readVoltage
    handle'' (AT ATResetAll) = reply <$> resetAll
    handle'' (AT (ATSelectProtocol p)) = reply <$> selectProtocol p
    handle'' (AT ATVersionID) = reply . (^. versionID) <$> get

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
replyOK :: Monad m => StateT (Simulator bus) m String
replyOK = return "OK"

-- | Connect to the underlying bus
--
-- If the connection fails, the result will be 'Left errorMessage', otherwise
-- the result will be 'Right statusMessage'.
connectToBus :: (Monad m, OBDBus bus) => StateT (Simulator bus) m (Either String String)
connectToBus = do
    cp     <- (^. connectedProtocol) <$> get
    bus    <- OBDBus.protocol . (^. obdBus) <$> get
    chosen <- (^. chosenProtocol) <$> get
    case (cp, chosen) of
        (NotConnected, AutomaticProtocol) -> do
            (connectedProtocol .~ AutomaticallyChosen bus) <$> get >>= put
            return $ Right "SEARCHING..."
        (NotConnected, _) ->
            if bus /= chosen
                then return $ Left "UNABLE TO CONNECT"
                else do (connectedProtocol .~ ManuallyChosen bus) <$> get >>= put
                        return $ Right ""
        (_, _) -> return $ Right ""

-- | Describe the protocol number
describeProtocolNumber :: Monad m => StateT (Simulator bus) m String
describeProtocolNumber = do
    cp <- (^. connectedProtocol) <$> get
    case cp of NotConnected -> return $ "A" ++ AutomaticProtocol ^. re protocol
               ManuallyChosen p -> return $ p ^. re protocol
               AutomaticallyChosen p -> return $ "A" ++ p ^. re protocol

-- | Turn echo off
echoOff :: Monad m => StateT (Simulator bus) m String
echoOff = (echo .~ EchoOff) <$> get >>= put >> replyOK

-- | Read the battery voltage
readVoltage :: Monad m => StateT (Simulator bus) m String
readVoltage = printf "%.1f" . (/~ volt) . (^. pin2Voltage) <$> get

-- | Fully reset the device
resetAll :: (Monad m, OBDBus bus) => StateT (Simulator bus) m String
resetAll = defaultSimulator . (^. obdBus) <$> get >>= put >> return ""

-- | Select a protocol
selectProtocol :: Monad m => Protocol -> StateT (Simulator bus) m String
selectProtocol p = (chosenProtocol .~ p) <$> get >>= put >> replyOK
