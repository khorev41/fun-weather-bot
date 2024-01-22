{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Control.Monad.IO.Class
import Control.Applicative
import WeatherApi as Api
import ForecastApi as FApi
import Data.Text (Text, pack)
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple
    ( startBot_,
      conversationBot,
      (<#),
      reply,
      replyText,
      toReplyMessage,
      BotApp(..),
      Eff,
      ReplyMessage(replyMessageReplyMarkup))
import Telegram.Bot.Simple.UpdateParser
import Data.List (intercalate)

type Item = Text

data Action
  = Start
  | Weather Text
  | ForecastAction Text
  deriving (Show, Read)

bot :: BotApp () Action
bot = BotApp
  { botInitialModel = ()
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }
  where
    updateToAction :: () -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
          Weather      <$> plainText
      <|> Start        <$  command "start"
      <|> Weather      <$> command "weather"
      <|> ForecastAction <$> command "forecast"
      <|> callbackQueryDataRead

    handleAction :: Action -> () -> Eff Action ()
    handleAction action _ = case action of
      Start -> () <# do
        reply (toReplyMessage startMessage)
          { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
      Weather city -> () <# do
        result <- liftIO (getWeatherMessage city)
        replyText result
      ForecastAction city -> () <# do
        result <- liftIO (getForecastMessage city)
        replyText result

      where
      startMessage = Text.unlines
          [ "Hello! I am your personal Fun-Weather-Bot :)"
          , ""
          , "I can provide you with current weather information!"
          , "Just send me the command /weather, and I will tell you how it is outside."
          , ""
          , "You can inquire about the weather in a specific city, for example:"
          , "/weather Bratislava or just Bratislava"
          , ""
          , "If you want to know the forecast for several days, use the command /forecast."
          , "Example: /forecast Košice"
          , ""
          , "Give it a try! Send me /weather city, and we'll see what's currently happening outside."
          ]

    startKeyboard :: ReplyKeyboardMarkup
    startKeyboard = ReplyKeyboardMarkup
      { replyKeyboardMarkupKeyboard =
          [ [ "Košice", "Bratislava" ]
          , [ "Poprad", "Žilina" ]
          ]
      , replyKeyboardMarkupResizeKeyboard = Just True
      , replyKeyboardMarkupOneTimeKeyboard = Just True
      , replyKeyboardMarkupSelective = Nothing
      , replyKeyboardMarkupInputFieldSelector = Nothing
      , replyKeyboardMarkupIsPersistent = Nothing
      }

getWeatherMessage :: Text -> IO Text
getWeatherMessage city = do
    print city
    maybeWeatherData <- Api.getWeatherData city
    case maybeWeatherData of
        Just weatherData -> return (pack ("*** " ++ Api.name (Api.location weatherData) ++ ", " ++ Api.country (Api.location weatherData) ++ " ***\n" ++
          "Temperature: " ++ show (Api.tempC (Api.current weatherData)) ++ "°C \n" ++
          "Feels like : " ++ show (Api.feelsLikeC (Api.current weatherData)) ++ "°C \n" ++
          "Wind: " ++ show (Api.windKph (Api.current weatherData)) ++ " km/h \n" ++
          "Condition: " ++ Api.conditionText (Api.condition (Api.current weatherData)) ++ " \n" ++
          "Cloudity: " ++ show (Api.cloud (Api.current weatherData)) ++ " \n" ++
          "Last updated: " ++ removeFirstAndLast (show (Api.lastUpdated (Api.current weatherData)))
          ))
        Nothing -> return "Error fetching weather data"


getForecastMessage :: Text -> IO Text
getForecastMessage city = do
    print city
    maybeForecastData <- FApi.getForecastData city
    case maybeForecastData of
        Just forecastData -> return (pack ("*** " ++ FApi.name (FApi.location forecastData) ++ ", " ++ FApi.country (FApi.location forecastData)++ " *** \n" ++
         concatenateStrings [ FApi.date dayInfo ++ "\n" ++
          "Minimum temperature: " ++ show (FApi.mintemp_c (day dayInfo)) ++ "°C \n" ++
          "Maximum temperature: " ++ show (FApi.maxtemp_c (day dayInfo)) ++ "°C \n" ++ 
          "Maximum wind speed: " ++ show (FApi.maxwind_kph (day dayInfo)) ++ " km/h \n" ++ 
          "Chance of rain: " ++ show (FApi.daily_chance_of_rain (day dayInfo)) ++ "% \n" ++ 
          "Chance of snow: " ++ show (FApi.daily_chance_of_snow (day dayInfo)) ++ "% \n" ++
          "Condition: " ++ FApi.text (FApi.condition (day dayInfo)) ++ "\n"| dayInfo <- FApi.forecastday (FApi.forecast forecastData)]  ))
        Nothing -> return "Error fetching weather data"


concatenateStrings :: [String] -> String
concatenateStrings = intercalate "\n --- \n"

removeFirstAndLast :: String -> String
removeFirstAndLast [] = []
removeFirstAndLast [_] = []
removeFirstAndLast (x:xs) = init xs

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId bot) env

main :: IO ()
main = do
    let token = "YOUR_API_KEY"
    putStr "Bot is running"
    run token
