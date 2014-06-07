module GeoTime.GoogleApi where

import Data.List (
  intercalate
  )
import Network.HTTP ( urlEncode )
import Text.JSON

type Parameter = (String, String)
type ApiURL = String
type Key = String

-- | > {apiURL}json?{p1}={v1}&...&{pn}={vn}
makeURL :: ApiURL -> [Parameter] -> String
makeURL url p = url ++ "json?" ++ params p
  where
    params = intercalate "&" . map assoc
    assoc (pName, pVal) = pName ++ "=" ++ urlEncode pVal

-- | \"sensor=false\"
sensorF :: Parameter
sensorF = ("sensor", "false")

apiKey :: Key -> Parameter
apiKey k = ("key", k)

maybeElts :: Eq a => [a] -> [(a, b)] -> [Maybe b]
maybeElts = flip (map . flip lookup)

allElts :: Eq a => [a] -> [(a, b)] -> Maybe [b]
allElts as = sequence . maybeElts as

fromJSObject' :: JSValue -> Maybe [(String, JSValue)]
fromJSObject' (JSObject jso) = Just $ fromJSObject jso
fromJSObject' _ = Nothing
