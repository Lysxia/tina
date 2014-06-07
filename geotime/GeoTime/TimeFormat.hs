module GeoTime.TimeFormat where

import Data.Time
import Data.Time.Calendar.WeekDate
import System.Locale

weekDay :: Day -> Int
weekDay day = let (_,_,d) = toWeekDate day in d

dayOfWeek :: Int -> String
dayOfWeek 0 = "Di"
dayOfWeek 1 = "Lu"
dayOfWeek 2 = "Ma"
dayOfWeek 3 = "Me"
dayOfWeek 4 = "Je"
dayOfWeek 5 = "Ve"
dayOfWeek 6 = "Sa"

someFormat = "%d/%m/%y %T %Z (TUC%z)"

timeString :: FormatTime t => t -> String
timeString = formatTime defaultTimeLocale someFormat

fullTimeString :: ZonedTime -> String
fullTimeString zt = dow ++ " " ++ timeString zt
  where dow = dayOfWeek . weekDay . localDay . zonedTimeToLocalTime $ zt

