{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import IRC
import Network
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Thread.Delay
import Options.Applicative hiding (action)
import System.IO
import System.Random

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as Text
import Data.Text.Encoding

data Options = Options
  { host :: String
  , port :: Int
  , chan :: String }

parseOptions = Options
  <$> strArgument ( metavar "HOSTNAME" )
  <*> argument auto ( metavar "PORT" )
  <*> strArgument ( metavar "#CHANNEL" )

main =
 do Options{..} <- execParser $ info (helper <*> parseOptions) briefDesc
    let server = Server host port
        c = B8.pack chan
    gla <- glassonBox
    forkIO $ glasson gla server c
    runIRC server $ do
      setUserNick "Glassiere" "distributeur" "Machine à glaçons"
      waitWelcome
      connectToChan c
      forever $ do
        msg <- listen
        case msg of
          Message _ "PRIVMSG" [target, txt]
            | let txt' = decodeUtf8 txt,
              target == c && "Glassiere" `Text.isPrefixOf` txt'
                          && "glaçon" `Text.isInfixOf` txt'
            -> void . liftIO $ putGlasson gla
          _ -> return ()

-- Wait for a glasson to melt a bit
delay' :: MonadIO m => m ()
delay' = liftIO $ do
  secs <- randomRIO (30, 90)
  delay $ secs * 10^6 -- in microseconds

-- Icebox
type Glassons = MVar ()

-- Create an icebox
glassonBox :: IO Glassons
glassonBox = newEmptyMVar

takeGlasson :: MonadIO m => Glassons -> m ()
takeGlasson = liftIO . takeMVar

putGlasson :: MonadIO m => Glassons -> m ()
putGlasson gla = void . liftIO $ tryPutMVar gla ()

-- Take glassons out of the icebox and let them melt.
glasson :: Glassons -> Server -> B8.ByteString -> IO ()
glasson gla server c =
  runIRC server $ do
    setUserNick "Le_glasson" "H2O" "Monoxyde de dihydrogène"
    waitWelcome
    mapIRC forkIO $ forever listen -- Discard any input
    forever $ do
      takeGlasson gla
      send $ nick "Le_glasson"
      connectToChan c
      delay'
      send $ action c "rafraîchit la salle."
      delay'
      send $ nick "Le_petit_glasson"
      delay'
      send $ action c "a fondu."
      send $ part c

