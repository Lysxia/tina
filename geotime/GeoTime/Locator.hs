module GeoTime.Locator where

import GeoTime.GoogleApi
import GeoTime.Geocode
import GeoTime.TimeZone
import GeoTime.Logging
import GeoTime.Time

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.Lazy

import Data.Time

-- | Parse two Doubles separated by spaces or commas
--
-- > maybeParsePosition "   1.2  , ,,3.4   " == Just (1.2, 3.4)
maybeParsePosition :: String -> Maybe (Double, Double)
maybeParsePosition s = mb
  where
    fst = case reads s of
            [] -> Nothing
            [ok] -> Just ok
    snd' (fst, s') = case reads $ dropWhile spaceOrComma s' of
                [] -> Nothing
                [(snd, _)] -> Just (fst, snd)
    mb = fst >>= snd'

spaceOrComma x = x == ' ' || x == ','

-- | First directly try to parse a latitude-longitude pair.
-- If it fails, submit the string to Google Geocode,
-- and return the position and address of a result, if any.
-- Fail otherwise.
locate
  :: Maybe FilePath -> Maybe Key -> String
  -> MaybeT IO (Maybe String, Position)
locate logDir k req
  = maybe (MaybeT query) (return . (,) Nothing)
  . maybeParsePosition $ req
  where
    query = do
      (ans, s) <- runWriterT $ askGeocode k req
      logReply s
      return $ getAnswer ans
    getAnswer [] = Nothing
    getAnswer (x : _) = Just (Just (formatted_address x), resultLocation x)
    logReply = maybeLogToFile logDir req

-- | Request current time zone from Google Time Zone.
timeZone :: Maybe FilePath -> Maybe Key
         -> Double -> Double -> MaybeT IO TimeZone
timeZone logDir k lat lon = do
  (ans, s) <- lift . runWriterT $ query
  logReply s
  MaybeT . return $ toTimeZone <$> ans
  where
    query = askTimeZone k lat lon =<< timestamp
    timestamp = lift $ secSinceEPOCH
    logReply = lift . maybeLogToFile logDir (show lat ++ "_" ++ show lon)

