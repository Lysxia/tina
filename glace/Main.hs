{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import IRC
import Network
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Thread.Delay
import Options.Applicative hiding (action)
import System.IO
import qualified Data.ByteString.Char8 as B8

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
    runIRC (Server host port) $ do
      let c = B8.pack chan
      setUserNick "Le_glasson" "H2O" "Monoxyde de dihydrogène"
      waitWelcome
      connectToChan c
      liftIO $ delay $ 1 * 10^6
      send $ action c "rafraîchit la salle."
      liftIO $ delay $ 1 * 10^6
      send $ nick "Le_petit_glasson"
      liftIO $ delay $ 1 * 10^6
      send $ action c "fond."
      send $ nick "L_eau_coolante"
      send $ part c
      send $ quit Nothing
      liftIO $ delay $ 10^5

