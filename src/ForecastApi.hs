{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module ForecastApi where


import WeatherApi (replaceSpacesWithPercent)
import Data.Aeson
import GHC.Generics
import Data.Text
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as LBS

data WeatherForecastData = WeatherForecastData
    { location :: Location
    , current :: Current
    , forecast :: Forecast
    } deriving (Generic, Show)

data Location = Location
    { name :: String
    , region :: String
    , country :: String
    , lat :: Double
    , lon :: Double
    , tz_id :: String
    , localtime_epoch :: Int
    , localtime :: String
    } deriving (Generic, Show)

data Current = Current
  { lastUpdatedEpoch :: Integer
  , lastUpdated :: String
  , tempC :: Float
  , tempF :: Float
  , isDay :: Int
  , conditionCurrent :: Condition
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
  , uvCurrent :: Float
  , gustMph :: Float
  , gustKph :: Float
  } deriving (Generic, Show)

data Condition = Condition
    { text :: String
    , icon :: String
    , code :: Int
    } deriving (Generic, Show)

data Forecast = Forecast
    { forecastday :: [ForecastDay]
    } deriving (Generic, Show)

data ForecastDay = ForecastDay
    { date :: String
    , date_epoch :: Int
    , day :: Day
    } deriving (Generic, Show)

data Day = Day
    { maxtemp_c :: Double
    , maxtemp_f :: Double
    , mintemp_c :: Double
    , mintemp_f :: Double
    , avgtemp_c :: Double
    , avgtemp_f :: Double
    , maxwind_mph :: Double
    , maxwind_kph :: Double
    , totalprecip_mm :: Double
    , totalprecip_in :: Double
    , totalsnow_cm :: Double
    , avgvis_km :: Double
    , avgvis_miles :: Double
    , avghumidity :: Int
    , daily_will_it_rain :: Int
    , daily_chance_of_rain :: Int
    , daily_will_it_snow :: Int
    , daily_chance_of_snow :: Int
    , condition :: Condition
    , uv :: Double
    } deriving (Generic, Show)

instance FromJSON WeatherForecastData
instance FromJSON Location
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
instance FromJSON Condition
instance FromJSON Forecast
instance FromJSON ForecastDay
instance FromJSON Day

getForecastData :: Text -> IO (Maybe WeatherForecastData)
getForecastData cityName = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest (replaceSpacesWithPercent ("https://api.weatherapi.com/v1/forecast.json?key=<API_KEY>&q=" ++ unpack cityName ++ "&days=3" ))
    response <- httpLbs request manager
    let jsonString = LBS.unpack $ responseBody response

    case eitherDecode (LBS.pack jsonString) of
        Left err -> do
            writeFile "output.txt" err
            return Nothing
        Right weatherData -> do
            return (Just weatherData)