{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module IRC
  ( IRC, mapIRC, runIRC, runIRC'
  , Server(..)
  , Message(..)
  , sender
  , connect
  , setUserNick
  , waitWelcome
  , connectToChan
  , listen, send
  , nick, part, quit, action, privmsg
  ) where

import Network
import Network.IRC
import Network.IRC.CTCP

import System.IO
import System.IO.Error
import System.Environment
import System.Exit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
--import qualified Data.ByteString.UTF8 as BU

import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Maybe
import Data.Char
import Data.Text.Encoding (decodeLatin1)

mapIRC :: (IO a -> IO b) -> IRC a -> IRC b
mapIRC = mapReaderT

mkMessage           :: ByteString -> [Parameter] -> Message
mkMessage cmd params = Message Nothing cmd params

logB8 :: ByteString -> IRC ByteString
logB8 s = do
  log <- ircLog <$> ask
  when log $ liftIO $ B8.putStrLn s
  return s

type ByteString = BS.ByteString

sender :: Message -> Maybe ByteString
sender (Message (Just (NickName s _ _)) _ _) = Just s
sender _ = Nothing

data Session = Session
  { ircHandle :: Handle
  , ircLog :: Bool
  }

ircHandle' = ircHandle <$> ask

type IRC = ReaderT Session IO

getLineIRC :: IRC ByteString
getLineIRC = do
  h <- ircHandle'
  s <- liftIO $ BS.hGetLine h
  logB8 s

putStrIRC :: ByteString -> IRC ()
putStrIRC s = do
  logB8 $ B8.cons '>' s
  h <- ircHandle'
  liftIO $ BS.hPutStr h (B8.append s "\r\n")

-- | Handles pings
listen :: IRC Message
listen =
 do s <- getLineIRC
    case decode s of
      Just (Message _ "PING" params) -> sendPong params >> listen
      Nothing -> listen
      Just m -> return m

send :: Message -> IRC ()
send msg =
 do let emsg = encode msg
    putStrIRC emsg

connectToChan :: ByteString -> IRC ()
connectToChan chan = send $ joinChan chan

setUserNick :: ByteString -> ByteString -> ByteString -> IRC ()
setUserNick nickname username realname = do
  send $ nick nickname
  send $ user username "0" "*" realname

waitWelcome :: IRC ()
waitWelcome = void $ iterateUntil welcome listen
  where
    welcome (Message _ "001" _) = True
    welcome _ = False

sendPong :: [ByteString] -> IRC ()
sendPong =
  putStrIRC . (`B8.append` "\r\n") . encode . pong . head

action :: Channel -> ByteString -> Message
action c =
  privmsg c . getUnderlyingByteString . toCTCP "ACTION" . (:[]) . decodeLatin1

partWith :: Channel -> ByteString -> Message
partWith c m = mkMessage "PART" [c, m]

data Server = Server String Int

connect :: Server -> IO Handle
connect (IRC.Server host port) =
 do h <- connectTo host (PortNumber . fromIntegral $ port)
    hSetBuffering h NoBuffering
    return h

runIRC :: Server -> IRC a -> IO a
runIRC = runIRC' False

runIRC' :: Bool -> Server -> IRC a -> IO a
runIRC' log server irc =
 do h <- connect server
    a <- runReaderT irc (Session h log)
    hClose h
    return a

