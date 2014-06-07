module GeoTime.Args where

import System.IO
import System.Environment
import System.Exit
import System.Directory

data Arguments = Arguments {
  opts :: Options,
  task :: Task
  }
  deriving Show

data Options = Options {
  key :: Maybe String,
  logDir :: Maybe FilePath
  }
  deriving Show

data Task
  = TZ Double Double
  | Loc String
  | LoTi String
  deriving Show

taskLocate :: String -> Maybe Task
taskLocate = Just . Loc

taskLocalTime :: String -> Maybe Task
taskLocalTime = Just . LoTi

taskTimeZone :: String -> String -> Maybe Task
taskTimeZone lat lon
  = case (reads lat, reads lon) of
      ([(lat, "")], [(lon, "")]) -> maybeTZ lat lon
      _ -> Nothing

defOpts = Options {
  key = Nothing,
  logDir = Nothing
  }

parseArgs :: [String] -> IO Arguments
parseArgs [] = printUsage stderr >> exitSuccess
parseArgs args = parse' defOpts args

parse' :: Options -> [String] -> IO Arguments
parse' a (h : _) | h == "-h" || h == "--help" = printUsage stdout >> exitSuccess
parse' a ("-k" : k : args) = parse' (a { key = Just k }) args

parse' a ("-o" : filePath : args) = do
  e <- doesDirectoryExist filePath
  if e
  then parse' (a { logDir = Just filePath }) args
  else noDir filePath
parse' a ["-z", lat, lon]
  = setTask a (taskTimeZone lat lon)
parse' a ["-l", s] = setTask a (taskLocate s)
parse' a s = setTask a . taskLocalTime . unwords $ s

printUsage h = hPutStrLn h . (++ us) =<< getProgName
  where us = " [-h|--help] [-k key] {[-z] lat lon | [-l] location}"

argError = do
  hPutStrLn stderr "Invalid argument(s)."
  printUsage stderr
  exitFailure

noDir filePath
  = hPutStrLn stderr ("No such directory: " ++ filePath) >> exitFailure

maybeTZ :: Double -> Double -> Maybe Task
maybeTZ lat lon
  | -90 <= lat && lat <= 90 && -180 < lon && lon <= 180 = Just (TZ lat lon)
  | otherwise = Nothing -- Also rejects NaN

setTask :: Options -> Maybe Task -> IO Arguments
setTask = maybe argError return .+ fmap . Arguments

infixr 9 .+
(.+) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.+) = (.).(.)

