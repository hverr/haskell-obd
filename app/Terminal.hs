{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.List (isSuffixOf)
import qualified Data.ByteString.Char8 as Char8

import System.Console.Haskeline (InputT, MonadException, RunIO(..),
                                 controlIO, runInputT,
                                 defaultSettings, getInputLine)

import System.Hardware.ELM327.Connection (Con, recvRaw, sendString)
import System.Hardware.ELM327.Simulator (connect, defaultSimulator)
import System.Hardware.ELM327.Simulator.OBDBus.VWPolo2007 (stoppedCarBus)


newtype Term a = Term { runTerm :: ReaderT Con IO a }
                      deriving (Functor, Applicative, Monad, MonadIO, MonadReader Con)

instance MonadException Term where
    controlIO f = join . liftIO $ f (RunIO return)

main :: IO ()
main = connect (defaultSimulator stoppedCarBus) >>= runTerminalWithConnection

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

exitConnection :: InputT Term ()
exitConnection = undefined

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
