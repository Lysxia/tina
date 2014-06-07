{-# LANGUAGE OverloadedStrings #-}
module Main where

{- Talking to this IRC bot runs a shell command with the given string as an
 - argument.
 - 
 - >$ ./ExecBot {nick} {owner} {passwd} {server} {port} {cmd} {chan}*
 -
 - connects ExecBot to {server}:{port}, and channels {chan}*,
 - using {nick} as its nickname.
 -
 - >< John> {nick} do something
 -
 - will run:
 -
 - >$ {cmd} 'do something'
 -
 - and ExecBot will reply with the first output line, if any:
 -
 - >< {nick}> {first line}
 -
 - The arguments {owner} {passwd} allow to give special commands
 - to ExecBot via PM.
 -
 - * Disconnect ExecBot
 -
 -   >< {owner}> {passwd} quit
 -
 - * Ask for the command run by ExecBot
 -
 -   >< {owner}> {passwd} show
 -
 - * Send arbitrary IRC messages
 -
 -   >< {owner}> {passwd} raw PRIVMSG #hashtag YOLO
 -
 -}

import Network
import Network.IRC

import System.IO
import System.IO.Error
import System.Process
import System.Environment
import System.Posix.Escape
import System.Exit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as BU

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.Maybe
import Data.Char

{- Debug flag -}
debug :: Bool
debug = False

type ByteString = BS.ByteString

data BotCommand
  = Quit | NewExec String {-Disabled-} | ShowExec | IrcMsg Message

data Bot = Bot {
  botName :: ByteString,
  owner :: ByteString,
  passwd :: ByteString,
  executable :: String
  }

data Init = Init {
  botState :: Bot,
  server :: String,
  port :: PortID,
  chans :: [ByteString]
  }

type ErrMsg = String
type BotIO = StateT Bot IO

main :: IO ()
main = do
  let leftFail = leftFailWith (printUsage stderr >> exitFailure)
  arg <- leftFail . parseInit =<< getArgs'
  h <- connectTo (server arg) . port $ arg
  hSetBuffering h NoBuffering
  setUserNick h (botName . botState $ arg)
  forM_ (chans arg) . connectToChan $ h
  statefulForever (execStateT $ listen h) (botState arg)

statefulForever :: Monad m => (a -> m a) -> a -> m b
statefulForever f a = f a >>= statefulForever f

listen :: Handle -> BotIO ()
listen h = maybeRecv =<< (lift . fmap decode . hGetLine' $ h)
  where
    maybeRecv = maybe (return ()) (recv h)
    hGetLine' h = do
      s <- BS.hGetLine h
      when debug $ B8.putStrLn s
      return s

recv :: Handle -> Message -> BotIO ()
recv h t@(Message pre cmd params)
  | cmd == "PING"    = lift $ sendPong h params
  | cmd == "PRIVMSG" = recvPrivMsg h t
  | otherwise = return ()

recvPrivMsg :: Handle -> Message -> BotIO ()
recvPrivMsg h t@(Message pre _ params) = get >>= process
  where
    [chan, msg] = params
    process bot
      | whisp && pass && isOwner && isJust botCmd
                  = obey h . fromJust $ botCmd
      | whisp     = return () -- TODO
      | highlight = lift $ processRequest (executable bot) h chan msg'
      | otherwise = return ()
      where
        pass = passwd bot `BS.isPrefixOf` msg
        botCmd = readCommand . B8.drop passwdLength $ msg
        whisp = chan == botName bot
        highlight = botName bot `BS.isPrefixOf` msg
        msg' = B8.dropWhile spaceOrColon . B8.drop nameLength $ msg
        nameLength = B8.length . botName $ bot
        isOwner = maybe False (owner bot ==) . sender $ t
        passwdLength = (1 +) . B8.length . passwd $ bot

processRequest :: FilePath -> Handle -> Channel -> ByteString -> IO ()
processRequest exec h chan msg
  = executeThen exec (BU.toString msg) (ifNull logEmpty sendMsg)
  where
    logEmpty = putStrLn "<Empty>"
    sendMsg = sendPrivMsg h chan . BU.fromString

ifNull :: b -> ([a] -> b) -> [a] -> b
ifNull b f [] = b
ifNull _ f as = f as

-- Continuation passing style
executeThen :: FilePath -> String -> (String -> IO ()) -> IO ()
executeThen exec msg continue = do
  (_, Just hOut, _, ph) <- createProcess process
  status <- waitForProcess ph
  case status of
    ExitSuccess -> hGetLine' hOut >>= continue 
    ExitFailure i -> hPutStrLn stderr (failMsg i)
  where
    hGetLine' h = catchIOError (hGetLine h) catchEOF
    catchEOF e | isEOFError e = return ""
               | otherwise = ioError e
    process = (shell cmd) { std_out = CreatePipe }
    failMsg i = cmd ++ ": Fail status " ++ show i
    cmd | null msg  = exec
        | otherwise = exec ++ " " ++ escape msg

obey :: Handle -> BotCommand -> BotIO ()
obey h Quit = lift $ send h (quit Nothing) >> exitSuccess
obey _ (NewExec exec) = modify (\b -> b { executable = exec })
obey h (IrcMsg msg) = lift $ send h msg
obey h ShowExec = do
  bot <- get
  lift . sendPrivMsg h (owner bot) . BU.fromString . executable $ bot

sender :: Message -> Maybe ByteString
sender (Message (Just (NickName s _ _)) _ _) = Just s
sender _ = Nothing

readCommand :: ByteString -> Maybe BotCommand
readCommand "quit" = Just Quit
readCommand "show" = Just ShowExec
readCommand x
  | "raw "  `B8.isPrefixOf` x = fmap IrcMsg . decode . BS.drop 4 $ x
-- Disabled
--  | "exec " `B8.isPrefixOf` x = Just . NewExec . BU.toString . BS.drop 5 $ x
readCommand _ = Nothing

parseInit :: [String] -> Either ErrMsg Init
parseInit (name : owner : passwd : server : port : exec : chans)
  | all (flip elem ['&','#','+','!'] . head) chans
  = case reads port of
      [(port, "")]
        -> Right $ makeInit
                     name owner passwd server port exec chans
      _ -> Left "Invalid port."
  | otherwise = Left "Invalid chan name. Must begin with '&#+!'."
parseInit _ = Left "Wrong number of arguments."

makeInit
  :: String -> String -> String -> String -> Integer -> FilePath -> [String]
  -> Init
makeInit name owner passwd server port exec chans
  = Init
      b
      server
      (PortNumber . fromInteger $ port)
      (map BU.fromString chans)
  where
    b = Bot
      (BU.fromString name)
      (BU.fromString owner)
      (BU.fromString passwd)
      exec

leftFailWith :: IO a -> Either ErrMsg a -> IO a
leftFailWith e (Left msg) = hPutStrLn stderr msg >> e
leftFailWith _ (Right a)  = return a

printUsage :: Handle -> IO ()
printUsage h = hPutStrLn h . (++ us) =<< getProgName
  where us = " [-h|--help]\
             \ {nick} {owner} {passwd} {server} {port} {cmd} {chan}*"

send :: Handle -> Message -> IO ()
send h msg = do
  B8.hPutStr h $ emsg `B8.append` "\r\n"
  B8.putStrLn $ B8.cons '>' emsg
  where emsg = encode msg

connectToChan :: Handle -> ByteString -> IO ()
connectToChan h chan = send h . joinChan $ chan

setUserNick :: Handle -> ByteString -> IO ()
setUserNick h name = do
  send h . nick $ name
  send h $ user name "0" "*" "execBot"

sendPong :: Handle -> [ByteString] -> IO ()
sendPong h = send h . pong . head

sendPrivMsg :: Handle -> Channel -> ByteString -> IO ()
sendPrivMsg h = send h .+ privmsg

infixr 9 .+
(.+) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.+) = (.).(.)

-- If "-h" is given as an argument, print help and exit.
getArgs' :: IO [String]
getArgs' = getArgs >>= parseHelp
  where
    parseHelp (h : _) | h == "-h" || h == "--help"
      = printUsage stdout >> exitSuccess
    parseHelp s = return s

spaceOrColon :: Char -> Bool
spaceOrColon x = isSpace x || x == ':'

{-
data Options = Options { }

defaultOptions = Options { }

parseArgs :: [String] -> Either ErrMsg (Options, Bot)
parseArgs args = withOptions defaultOptions $ parseBot args

withOptions = fmap . (,)
-}
