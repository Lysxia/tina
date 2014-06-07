module GeoTime.Geocode where

import GeoTime.GoogleApi

import System.IO
import Network.HTTP.Conduit ( simpleHttp )
import Text.JSON
import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer.Lazy
import qualified Data.ByteString.Lazy.UTF8 as BU

googleGeocode = "https://maps.googleapis.com/maps/api/geocode/"

address :: String -> Parameter
address = (,) "address"

type WriterS = WriterT String

data GResult = GResult {
  types              :: [String],
  formatted_address  :: String,
  address_components :: [AddressComponent],
  geometry           :: Geometry
  }
  deriving Show

data AddressComponent = AC {
  acTypes  :: [String],
  long_name :: String,
  short_name :: String
  }
  deriving Show

-- Latitude, longitude
type Position = (Double, Double)

data Box = Box {
  ne :: Position,
  sw :: Position
  }
  deriving Show

data Geometry = Geometry {
  location      :: Position,
  location_type :: String,
  viewport      :: Box,
  bounds        :: Maybe Box
  }
  deriving Show

resultLocation :: GResult -> Position
resultLocation = location . geometry

parseResults :: String -> Either String [GResult]
parseResults = resultToEither . decode >=> toGResults
  where
    toGResults (JSObject jso) =
      let
        assoc = fromJSObject jso
        Just (JSString status) = lookup "status" assoc
        ~(Just (JSArray results)) = lookup "results" assoc
      in
      case fromJSString status of
        "OK" -> Right $ mapMaybe (toGResult <=< fromJSObject') results
        s -> Left  $ "Status: " ++ s
    toGResults _ = Left "Expected JSObject"

toGResult :: [(String, JSValue)] -> Maybe GResult
toGResult assoc = do
  [ JSArray  types' ,
    JSString fa     ,
    JSArray  acs    ,
    JSObject geo    ]
    <- allElts ["types", "formatted_address", "address_components", "geometry"]
         assoc
  geo' <- toGeometry $ fromJSObject geo
  return $
    GResult
      [fromJSString t | JSString t <- types']
      (fromJSString fa)
      (mapMaybe (toAddressComponent <=< fromJSObject') acs)
      geo'

toAddressComponent :: [(String, JSValue)] -> Maybe AddressComponent
toAddressComponent assoc = do
  [ JSArray  types' ,
    JSString long   ,
    JSString short  ]
    <- allElts ["types", "long_name", "short_name"] assoc
  return $
    AC
      [fromJSString t | JSString t <- types']
      (fromJSString long)
      (fromJSString short)

toGeometry :: [(String, JSValue)] -> Maybe Geometry
toGeometry assoc = do
  [ JSObject loc      ,
    JSString loc_type ,
    JSObject vp       ]
    <- allElts ["location", "location_type", "viewport"] assoc
  loc' <- toPosition $ fromJSObject loc
  vp' <- toBox $ fromJSObject vp
  let
    loc_type' = fromJSString loc_type
    bounds' = toBox =<< fromJSObject' =<< lookup "bounds" assoc
  return $ Geometry loc' loc_type' vp' bounds'

toBox :: [(String, JSValue)] -> Maybe Box
toBox assoc = do
  [ne, sw] <- sequence
            . map (toPosition <=< fromJSObject')
            =<< allElts ["northeast", "southwest"] assoc
  return $ Box ne sw
  where toPosition' = toPosition . fromJSObject

toPosition :: [(String, JSValue)] -> Maybe Position
toPosition assoc = do
  [ JSRational _ lat, JSRational _ lng ]
    <- allElts ["lat", "lng"] assoc
  return $ (fromRational lat, fromRational lng)

askGeocode :: Maybe Key -> String -> WriterS IO [GResult]
askGeocode k addr = do
  s <- lift $ BU.toString `fmap` simpleHttp url
  let gr = parseResults s
  tell s
  return . either (const []) id $ gr
  where
    url = makeURL googleGeocode params
    params = case k of
      Nothing -> params'
      Just k -> params' ++ [apiKey k]
    params' = [address addr, sensorF]

