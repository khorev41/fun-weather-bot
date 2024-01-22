{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WeatherApi where

import Data.Text(Text, unpack, pack, replace)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as LBS

data Condition = Condition
  { conditionText :: String
  , conditionIcon :: String
  , conditionCode :: Int
  } deriving (Generic, Show)

instance FromJSON Condition where
  parseJSON = withObject "Condition" $ \v ->
    Condition
      <$> v .: "text"
      <*> v .: "icon"
      <*> v .: "code"

data Current = Current
  { lastUpdatedEpoch :: Integer
  , lastUpdated :: String
  , tempC :: Float
  , tempF :: Float
  , isDay :: Int
  , condition :: Condition
  , windMph :: Float
  , windKph :: Float
  , windDegree :: Int
  , windDir :: String
  , pressureMb :: Float
  , pressureIn :: Float
  , precipMm :: Float
  , precipIn :: Float
  , humidity :: Int
  , cloud :: Int
  , feelsLikeC :: Float
  , feelsLikeF :: Float
  , visKm :: Float
  , visMiles :: Float
  , uv :: Float
  , gustMph :: Float
  , gustKph :: Float
  } deriving (Generic, Show)

instance FromJSON Current where
  parseJSON = withObject "Current" $ \v ->
    Current
      <$> v .: "last_updated_epoch"
      <*> v .: "last_updated"
      <*> v .: "temp_c"
      <*> v .: "temp_f"
      <*> v .: "is_day"
      <*> v .: "condition"
      <*> v .: "wind_mph"
      <*> v .: "wind_kph"
      <*> v .: "wind_degree"
      <*> v .: "wind_dir"
      <*> v .: "pressure_mb"
      <*> v .: "pressure_in"
      <*> v .: "precip_mm"
      <*> v .: "precip_in"
      <*> v .: "humidity"
      <*> v .: "cloud"
      <*> v .: "feelslike_c"
      <*> v .: "feelslike_f"
      <*> v .: "vis_km"
      <*> v .: "vis_miles"
      <*> v .: "uv"
      <*> v .: "gust_mph"
      <*> v .: "gust_kph"

data Location = Location
  { name :: String
  , region :: String
  , country :: String
  , lat :: Float
  , lon :: Float
  , tzId :: String
  , localtimeEpoch :: Integer
  , localtime :: String
  } deriving (Generic, Show)

instance FromJSON Location where
  parseJSON = withObject "Location" $ \v ->
    Location
      <$> v .: "name"
      <*> v .: "region"
      <*> v .: "country"
      <*> v .: "lat"
      <*> v .: "lon"
      <*> v .: "tz_id"
      <*> v .: "localtime_epoch"
      <*> v .: "localtime"

data WeatherData = WeatherData
  { location :: Location
  , current :: Current
  } deriving (Generic, Show)

instance FromJSON WeatherData

replaceSpacesWithPercent :: String -> String
replaceSpacesWithPercent input = unpack $ replace (pack " ") (pack "%20") (pack input)

getWeatherData :: Text -> IO (Maybe WeatherData)
getWeatherData cityName = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest (replaceSpacesWithPercent ("http://api.weatherapi.com/v1/current.json?key=<API_KEY>&q=" ++ unpack cityName))
    response <- httpLbs request manager
    let jsonString = LBS.unpack $ responseBody response
    
    case eitherDecode (LBS.pack jsonString) of
        Left err -> do
            -- writeFile "output.txt" err
            return Nothing
        Right weatherData -> do
            return (Just weatherData)
            
