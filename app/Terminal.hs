{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Exception (bracket)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
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

import System.Hardware.ELM327.Connection (Con, ConT, ConError, withCon, recv, sendString)
import System.Hardware.ELM327.Simulator (defaultSimulator)
import System.Hardware.ELM327.Simulator.OBDBus.VWPolo2007 (stoppedCarBus)
import qualified System.Hardware.ELM327.Connection as Connection
import qualified System.Hardware.ELM327 as ELM327
import qualified System.Hardware.ELM327.Simulator as Simulator


newtype TermT a = TermT { runTermT :: ConT IO a }
                        deriving (Functor, Applicative, Monad, MonadIO)

instance MonadException TermT where
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
connect ConnectionTypeSimulator          = Simulator.connect (defaultSimulator stoppedCarBus)
connect (ConnectionTypeActualDevice dev) = either (\e -> fail $ "could not connect: " ++ show e) return =<< ELM327.connect dev

runTerminal :: ConnectionType -> IO ()
runTerminal ct = do
    r <- bracket (connect ct)
                 (\c -> putStrLn "Closing connection..." >> Connection.close' c)
                 (liftIO . runTerminalWithConnection)
    either (\e -> fail $ "error: " ++ show e) return r

runTerminalWithConnection :: Con -> IO (Either ConError ())
runTerminalWithConnection con = withCon con (runTermT readEvalPrint)

readEvalPrint :: TermT ()
readEvalPrint = runInputT defaultSettings loop
  where
    loop :: InputT TermT ()
    loop = do
        maybeLine <- getInputLine "% "
        case maybeLine of Nothing -> return ()
                          Just ":exit" -> return ()
                          Just x -> handleLine x >> loop

handleLine :: String -> InputT TermT ()
handleLine cmd = do
    lift $ TermT . sendString $ cmd ++ "\r"
    printAll

printAll :: InputT TermT ()
printAll = do
    maybeBS <- lift $ TermT recv
    case maybeBS of Nothing -> return ()
                    Just b -> liftIO . putStrLn $ convertResponse b
  where
    convertResponse = stripPrompt . filter (/= '\0') . Char8.unpack
    stripPrompt :: String -> String
    stripPrompt x = if "\n>" `isSuffixOf` x then take (length x - 1) x else x
