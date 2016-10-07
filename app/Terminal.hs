{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Exception (bracket)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.List (isSuffixOf)
import qualified Data.ByteString.Char8 as Char8

import Options.Applicative (Parser, execParser,
                            flag, strOption,
                            help, long, value,
                            info, progDesc,
                            (<>))
import Options.Applicative.Extra (helper)

import System.Console.Haskeline (InputT, MonadException, RunIO(..),
                                 controlIO, runInputT,
                                 defaultSettings, getInputLine)

import System.Hardware.ELM327.Connection (Con, recvRaw, sendString)
import System.Hardware.ELM327.Simulator (defaultSimulator)
import System.Hardware.ELM327.Simulator.OBDBus.VWPolo2007 (stoppedCarBus)
import qualified System.Hardware.ELM327.Connection as Connection
import qualified System.Hardware.ELM327 as ELM327
import qualified System.Hardware.ELM327.Simulator as Simulator


newtype Term a = Term { runTerm :: ReaderT Con IO a }
                      deriving (Functor, Applicative, Monad, MonadIO, MonadReader Con)

instance MonadException Term where
    controlIO f = join . liftIO $ f (RunIO return)

data ConnectionType = ConnectionTypeActualDevice FilePath
                    | ConnectionTypeSimulator

connectionType :: Parser ConnectionType
connectionType = flag ConnectionTypeActualDevice (const ConnectionTypeSimulator) m <*> serialPort
    where m = long "simulator" <> help "Connect to the simulator instead of a serial port"

serialPort :: Parser FilePath
serialPort = strOption m
    where m = long "port" <> value "/dev/ttyUSB0" <> help "The serial port to connect to"

main :: IO ()
main = execParser i >>= runTerminal
    where i = info p $ progDesc "Connect a terminal to an ELM327 device or simulator."
          p = helper <*> connectionType

connect :: ConnectionType -> IO Con
connect ConnectionTypeSimulator          = Simulator.connect $ defaultSimulator stoppedCarBus
connect (ConnectionTypeActualDevice dev) = ELM327.connect dev

runTerminal :: ConnectionType -> IO ()
runTerminal ct = bracket (connect ct)
                         (\c -> putStrLn "Closing connection..." >> Connection.close c)
                         (liftIO . runTerminalWithConnection)

runTerminalWithConnection :: Con -> IO ()
runTerminalWithConnection = runReaderT (runTerm readEvalPrint)

readEvalPrint :: Term ()
readEvalPrint = runInputT defaultSettings loop
  where
    loop :: InputT Term ()
    loop = do
        maybeLine <- getInputLine "% "
        case maybeLine of Nothing -> return ()
                          Just ":exit" -> return ()
                          Just x -> handleLine x >> loop

handleLine :: String -> InputT Term ()
handleLine cmd = do
    con <- lift ask
    liftIO $ sendString con $ cmd ++ "\r"
    printAll

printAll :: InputT Term ()
printAll = do
    maybeBS <- lift ask >>= liftIO . recvRaw
    case maybeBS of Nothing -> return ()
                    Just b -> liftIO . putStr $ convertResponse b
  where
    convertResponse = stripPrompt . map convCR . filter (/= '\0') . Char8.unpack
    convCR '\r' = '\n'
    convCR x = x

    stripPrompt :: String -> String
    stripPrompt x = if "\n>" `isSuffixOf` x then take (length x - 1) x else x
