module Airbnbeast.WebServer where

import Prelude

import Airbnbeast.Availability (Apartment(..), fetchGuestStays)
import Airbnbeast.Cleaning as Cleaning
import Airbnbeast.Html as Html
import Airbnbeast.Storage (Storage)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (nowDate)
import HTTPure (Request, ResponseM, ServerM, header, notFound, ok', serve, found')
import HTTPure.Method (Method(..))
import HTTPure.Body (toString)
import Data.String as String
import Data.Array as Array
import Data.Int as Int
import Data.Date (Date)
import Data.Date as Date
import Data.Enum (toEnum, fromEnum)
import Airbnbeast.Cleaning (TimeOfDay(..), TimeBlock(..))
import Airbnbeast.Availability (GuestStay)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

type Routes = Request -> ResponseM

normalizeApartmentName :: String -> Maybe Apartment
normalizeApartmentName "gloria" = Just (Apartment "Glória")
normalizeApartmentName "santa" = Just (Apartment "Santa")
normalizeApartmentName _ = Nothing

parseFormData :: String -> Maybe { apartment :: Apartment, date :: Date, timeOfDay :: TimeOfDay }
parseFormData formData = do
  let pairs = String.split (String.Pattern "&") formData
  apartmentStr <- getFormValue "apartment" pairs
  dateStr <- getFormValue "date" pairs
  timeOfDayStr <- getFormValue "timeOfDay" pairs
  
  apartment <- normalizeApartmentName apartmentStr
  date <- parseDate dateStr
  timeOfDay <- parseTimeOfDay timeOfDayStr
  
  pure { apartment, date, timeOfDay }

getFormValue :: String -> Array String -> Maybe String
getFormValue key pairs = do
  let keyPrefix = key <> "="
  pair <- Array.find (String.contains (String.Pattern keyPrefix)) pairs
  String.stripPrefix (String.Pattern keyPrefix) pair

parseDate :: String -> Maybe Date
parseDate dateStr = 
  case String.split (String.Pattern "-") dateStr of
    [yearStr, monthStr, dayStr] -> do
      year <- toEnum =<< Int.fromString yearStr
      month <- toEnum =<< Int.fromString monthStr  
      day <- toEnum =<< Int.fromString dayStr
      pure $ Date.canonicalDate year month day
    _ -> Nothing

parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay "Morning" = Just Morning
parseTimeOfDay "Afternoon" = Just Afternoon
parseTimeOfDay _ = Nothing

createTimeBlock :: Apartment -> Date -> TimeOfDay -> Boolean -> TimeBlock
createTimeBlock apartment date timeOfDay available =
  let dummyStay = 
        { apartment: apartment
        , fromDate: date
        , toDate: date
        , last4Digits: "0000"
        , link: ""
        }
  in
    TimeBlock
      { date: date
      , timeOfDay: timeOfDay
      , available: available
      , stay: dummyStay
      }

routes :: Storage -> Routes
routes storage { method: Get, path: [] } = do
  liftEffect $ log "Serving full cleaning schedule"
  schedule <- fetchCleaningSchedule
  ok' (header "Content-Type" "text/html") (Html.cleaningSchedulePage schedule)

routes storage { method: Get, path: [ "apartment", apartmentName ] } = do
  liftEffect $ log $ "Serving apartment page for: " <> apartmentName
  schedule <- fetchCleaningSchedule
  case normalizeApartmentName apartmentName of
    Just apartment ->
      case Map.lookup apartment schedule of
        Just windows ->
          ok' (header "Content-Type" "text/html") (Html.apartmentPage apartment windows)
        Nothing ->
          notFound
    Nothing ->
      notFound

routes storage { method: Get, path: [ "tailwind.css" ] } = do
  liftEffect $ log "Serving Tailwind CSS"
  result <- attempt $ readTextFile UTF8 "./dist/tailwind.css"
  case result of
    Right css ->
      ok' (header "Content-Type" "text/css") css
    Left _ ->
      notFound

routes storage { method: Get, path: [ "application.js" ] } = do
  liftEffect $ log "Serving JavaScript application"
  result <- attempt $ readTextFile UTF8 "./dist/application.js"
  case result of
    Right js ->
      ok' (header "Content-Type" "application/javascript") js
    Left _ ->
      notFound

routes storage { method: Patch, path: [ "timeblocks", "enable" ], body } = do
  liftEffect $ log "Enabling time block"
  bodyText <- toString body
  case parseFormData bodyText of
    Just { apartment, date, timeOfDay } -> do
      let timeBlock = createTimeBlock apartment date timeOfDay true
      _ <- attempt $ storage.enableTimeBlock timeBlock
      found' (header "Location" "/") "/"
    Nothing ->
      found' (header "Location" "/") "/"

routes storage { method: Patch, path: [ "timeblocks", "disable" ], body } = do
  liftEffect $ log "Disabling time block"
  bodyText <- toString body
  case parseFormData bodyText of
    Just { apartment, date, timeOfDay } -> do
      let timeBlock = createTimeBlock apartment date timeOfDay false
      _ <- attempt $ storage.disableTimeBlock timeBlock
      found' (header "Location" "/") "/"
    Nothing ->
      found' (header "Location" "/") "/"

routes _ _ = do
  liftEffect $ log "404 - Page not found"
  notFound

fetchCleaningSchedule :: Aff (Map.Map Apartment (Array Cleaning.CleaningWindow))
fetchCleaningSchedule = do
  let
    gloria =
      { apartment: Apartment "Glória"
      , icsUrl: "https://www.airbnb.com/calendar/ical/47420131.ics?s=317a8c2653c6a64a4cc4ed1ad89f6afd"
      }
    santa =
      { apartment: Apartment "Santa"
      , icsUrl: "https://www.airbnb.com/calendar/ical/568955469596249266.ics?s=878a19388f719853be173bf4ad3dd77c"
      }

  currentDate <- liftEffect nowDate
  guestStays <- fetchGuestStays [ gloria, santa ]
  pure $ Cleaning.scheduleFromGuestStaysWithDate currentDate guestStays

startServer :: { storage :: Storage, port :: Int } -> ServerM
startServer { port, storage } = do
  log $ "🚀 Airbnbeast web server starting on port " <> show port
  log "📋 Available routes:"
  log "  / - Full cleaning schedule"
  log "  /apartment/:name - Apartment-specific schedule"
  log "  PATCH /timeblocks/enable - Enable time block"
  log "  PATCH /timeblocks/disable - Disable time block"
  log ""
  serve port (routes storage) do
    log $ "✅ Server is running on http://localhost:" <> show port