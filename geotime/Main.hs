{-# LANGUAGE OverloadedStrings #-}
module Main where

import GeoTime.Locator
import GeoTime.Time
import GeoTime.TimeFormat
import GeoTime.Logging
import GeoTime.Args

import System.IO
import System.Environment
import System.FilePath
import System.Exit

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

main = do
  getArgs >>= parseArgs >>= doTask
  exitSuccess

doTask :: Arguments -> IO ()
doTask (Arguments (Options k logD) task)
  = case task of
      Loc s -> runMaybeT (locate' s) >>= print
      TZ lat lon -> runMaybeT (timeZone' lat lon) >>= print
      LoTi s -> (>> return ()) . runMaybeT $ do
              (addr, pos) <- locate' s
              uncurry timeZone' pos
                >>= lift . getCurrentZonedTime
                >>= lift . putStrLn . maybeAppend addr . fullTimeString
   where
     maybeAppend Nothing = id
     maybeAppend (Just a) = (++ " @ " ++ a)
     locate' = locate logD k
     timeZone' = timeZone logD k

