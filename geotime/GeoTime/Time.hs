module GeoTime.Time where

import GeoTime.TimeZone ( GTimeZone (..) )

import Data.Time
import Data.Time.Clock.POSIX

getTimeZone :: GTimeZone -> TimeZone
getTimeZone gtz
  = TimeZone {
      timeZoneMinutes    = ofs `div` 60,
      timeZoneSummerOnly = dstOffset gtz /= 0,
      timeZoneName       = name
      }
  where
    ofs = fromInteger $ dstOffset gtz + rawOffset gtz
    name = map head . words $ gTimeZoneName gtz

getCurrentZonedTime :: TimeZone -> IO ZonedTime
getCurrentZonedTime = flip fmap getCurrentTime . utcToZonedTime 

