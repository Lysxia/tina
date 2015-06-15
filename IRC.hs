{-# LANGUAGE OverloadedStrings #-}
module IRC
  ( IRC
  , runIRC
  , Server(..)
  , connect
  , setUserNick
  , waitWelcome
  , connectToChan
  , listen
  , send
  , nick
  , part
  , quit
  , action
  , privmsg
  , sender
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

mkMessage           :: ByteString -> [Parameter] -> Message
mkMessage cmd params = Message Nothing cmd params

{- Debug flag -}
debug :: Bool
debug = True

logB8 = when debug . B8.putStrLn

type ByteString = BS.ByteString

sender :: Message -> Maybe ByteString
sender (Message (Just (NickName s _ _)) _ _) = Just s
sender _ = Nothing

type IRC = ReaderT Handle IO

getLineIRC :: IRC ByteString
getLineIRC = liftIO . BS.hGetLine =<< ask

putStrIRC :: ByteString -> IRC ()
putStrIRC s =
  ask >>= \h -> liftIO $ BS.hPutStr h (B8.append s "\r\n")

-- | Handles pings
listen :: IRC (Maybe Message)
listen =
 do s <- getLineIRC
    liftIO $ logB8 s
    case decode s of
      Just (Message _ "PING" params) -> sendPong params >> listen
      m -> return m

send :: Message -> IRC ()
send msg =
 do let emsg = encode msg
    liftIO $ logB8 $ B8.cons '>' emsg
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
    welcome (Just (Message _ "001" _)) = True
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
runIRC server irc =
 do h <- connect server
    a <- runReaderT irc h
    hClose h
    return a

