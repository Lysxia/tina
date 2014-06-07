module GeoTime.TimeZone where

import GeoTime.GoogleApi

import System.IO
import Network.HTTP.Conduit ( simpleHttp )

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer.Lazy
import qualified Data.ByteString.Lazy.UTF8 as BU
import Data.List ( intercalate )
import Data.Time
import Data.Time.Clock.POSIX
import Text.JSON

googleTimeZone = "https://maps.googleapis.com/maps/api/timezone/"

location :: Double -> Double -> Parameter
location lat lng = ("location", show lat ++ "," ++ show lng)

type ByteString = BU.ByteString
type WriterS = WriterT String

type Timestamp = Integer

timestamp :: Timestamp -> Parameter
timestamp utc = ("timestamp", show utc)

currentTimestamp :: IO Parameter
currentTimestamp = fmap (timestamp . round) getPOSIXTime

data GTimeZone = GTimeZone {
  dstOffset :: Integer,
  rawOffset :: Integer,
  gTimeZoneID :: String,
  gTimeZoneName :: String
  }
  deriving Show

toTimeZone :: GTimeZone -> TimeZone
toTimeZone gtz = TimeZone (fromInteger minOfs) dst name
  where
    minOfs = (dstOffset gtz + rawOffset gtz) `div` 60
    dst = dstOffset gtz /= 0
    name = map head . words . gTimeZoneName $ gtz

assocToTZ :: [(String,JSValue)] -> Maybe GTimeZone
assocToTZ assoc = do
  [ JSRational _ dstR    ,
    JSRational _ rawR    ,
    JSString     tzIDS   ,
    JSString     tzNameS ]
    <- allElts ["dstOffset", "rawOffset", "timeZoneId", "timeZoneName"] assoc
  return $
    GTimeZone
      (round dstR)
      (round rawR)
      (fromJSString tzIDS)
      (fromJSString tzNameS)

parseTZ :: String -> Either String GTimeZone
parseTZ = resultToEither . decode >=> toTZ
  where
    toTZ (JSObject jso) =
      let assoc = fromJSObject jso in
      case fromJSString' $ lookup "status" assoc of
        "OK" -> maybe (Left "Object mismatch") Right $ assocToTZ assoc
        s    -> Left  $ "Status: " ++ s
    toTZ _ = Left "Expected JSObject"
    fromJSString' (Just (JSString s)) = fromJSString s

approxParis = location 48.86 2.333

askTimeZone
  :: Maybe Key -> Double -> Double -> Timestamp
  -> WriterS IO (Maybe GTimeZone)
askTimeZone k lat lng ts = do
  -- For some reason, I don't get MonadIO...?
  s <- lift $ BU.toString `fmap` simpleHttp url
  let tz = parseTZ s
  tell s
  return . either (const Nothing) Just $ tz
  where
    url = makeURL googleTimeZone params
    params = case k of
      Nothing -> params'
      Just k -> params' ++ [apiKey k]
    params' = [location lat lng, timestamp ts, sensorF]

