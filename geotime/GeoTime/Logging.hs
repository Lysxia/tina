module GeoTime.Logging where

import Data.Char
import Data.Time.Clock.POSIX
import System.IO
import System.FilePath

type UniqueHandle = String -> IO Handle

secSinceEPOCH :: IO Integer
secSinceEPOCH = fmap round getPOSIXTime

hLog :: Handle -> String -> String -> IO ()
hLog h t s = do
  i <- secSinceEPOCH
  hPutStrLn h $ show i ++ ":" ++ t' ++ ":" ++ s
  where
    t' | length t > n = take n t
       | otherwise    = replicate (n - length t) ' ' ++ t
    n = 6

logFile :: FilePath -> UniqueHandle
logFile path name = do
  s <- secSinceEPOCH
  let filePath = path </> (clean name ++ '_' : show s)
  openFile filePath WriteMode
  where
    clean = take 20 . map spaceToUL . unwords . words . map onlyAlphaNum
    onlyAlphaNum x
      | isAlphaNum x = x
      | otherwise    = ' '
    spaceToUL x
      | x == ' ' = '_'
      | otherwise = x

logToFile :: FilePath -> String -> String -> IO ()
logToFile path name s = do
  h <- logFile path name
  hPutStr h s
  hClose h

maybeLogToFile :: Maybe FilePath -> String -> String -> IO ()
maybeLogToFile = maybe (const . const $ return ()) logToFile
