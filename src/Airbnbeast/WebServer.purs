module Airbnbeast.WebServer where

import Prelude

import Airbnbeast.Availability (Apartment(..), fetchGuestStays)
import Airbnbeast.Cleaning as Cleaning
import Airbnbeast.Html as Html
import Airbnbeast.I18n as I18n
import Airbnbeast.Storage (Storage)
import Airbnbeast.Auth (AuthError(..))
import Data.Either (Either(..))
import Data.Map as Map
import Foreign.Object as Object
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (nowDate)
import HTTPure (Request, ResponseM, ServerM, header, notFound, ok', serve, found')
import HTTPure.Body (toString)
import HTTPure.Method (Method(..))
import Data.String as String
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Int as Int
import Data.Date (Date)
import Data.Date as Date
import Data.Traversable (traverse)
import Data.Enum (toEnum)
import Airbnbeast.Cleaning (TimeOfDay(..), TimeBlock(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

type Routes = Request -> ResponseM

-- Simple session management - in production you'd want more robust session handling
isAuthenticated :: Request -> Boolean
isAuthenticated request =
  -- For now, just check for a basic "session" cookie
  -- In production, you'd validate the session against a database
  -- This is a simplified check - in a real app you'd properly parse cookies
  String.contains (String.Pattern "session=authenticated") (show request.headers)

requireAuth :: Request -> ResponseM -> ResponseM
requireAuth request action =
  if isAuthenticated request then action
  else found' (header "Location" "/login") "/login"

parseLoginForm :: String -> Maybe { username :: String, password :: String }
parseLoginForm formData = do
  let pairs = String.split (String.Pattern "&") formData
  username <- getFormValue "username" pairs
  password <- getFormValue "password" pairs
  pure { username, password }

getFormValue :: String -> Array String -> Maybe String
getFormValue key pairs = do
  let keyPrefix = key <> "="
  pair <- Array.find (String.contains (String.Pattern keyPrefix)) pairs
  String.stripPrefix (String.Pattern keyPrefix) pair

normalizeApartmentName :: String -> Maybe Apartment
normalizeApartmentName "gloria" = Just (Apartment "Glória")
normalizeApartmentName "santa" = Just (Apartment "Santa")
normalizeApartmentName _ = Nothing

parsePathParameters :: String -> String -> String -> Maybe { apartment :: Apartment, date :: Date, timeOfDay :: TimeOfDay }
parsePathParameters apartmentStr dateStr timeOfDayStr = do
  apartment <- normalizeApartmentName apartmentStr
  date <- parseDate dateStr
  timeOfDay <- parseTimeOfDay timeOfDayStr
  pure { apartment, date, timeOfDay }

parseDate :: String -> Maybe Date
parseDate dateStr =
  case String.split (String.Pattern "-") dateStr of
    [ yearStr, monthStr, dayStr ] -> do
      year <- toEnum =<< Int.fromString yearStr
      month <- toEnum =<< Int.fromString monthStr
      day <- toEnum =<< Int.fromString dayStr
      pure $ Date.canonicalDate year month day
    _ -> Nothing

parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay "morning" = Just Morning
parseTimeOfDay "afternoon" = Just Afternoon
parseTimeOfDay "Morning" = Just Morning -- backward compatibility
parseTimeOfDay "Afternoon" = Just Afternoon -- backward compatibility
parseTimeOfDay _ = Nothing

-- Find a cleaning window that contains the specified TimeBlock
findCleaningWindowByTimeBlock :: TimeBlock -> Map.Map Apartment (Array Cleaning.CleaningWindow) -> Maybe Cleaning.CleaningWindow
findCleaningWindowByTimeBlock (TimeBlock { apartment, date, timeOfDay }) schedule = do
  windows <- Map.lookup apartment schedule
  Array.find (windowContainsTimeBlock date timeOfDay) windows
  where
  windowContainsTimeBlock :: Date -> TimeOfDay -> Cleaning.CleaningWindow -> Boolean
  windowContainsTimeBlock targetDate targetTimeOfDay (Cleaning.CleaningWindow { timeBlocks }) =
    let
      timeBlocksArray = NEArray.toArray timeBlocks
    in
      Array.any (\(Cleaning.TimeBlock tb) -> tb.date == targetDate && tb.timeOfDay == targetTimeOfDay) timeBlocksArray

createTimeBlock :: Apartment -> Date -> TimeOfDay -> Boolean -> TimeBlock
createTimeBlock apartment date timeOfDay available =
  TimeBlock
    { date: date
    , timeOfDay: timeOfDay
    , available: available
    , apartment: apartment
    }

routes :: Storage -> Routes
-- Login routes
routes _ { method: Get, path: [ "login" ], query } = do
  liftEffect $ log "Serving login page"
  let
    errorMsg = case Object.lookup "error" query of
      Just "invalid_credentials" -> Just I18n.pt.invalidCredentials
      Just "server_error" -> Just I18n.pt.serverError
      Just "invalid_data" -> Just I18n.pt.invalidLoginData
      _ -> Nothing
  ok' (header "Content-Type" "text/html") (Html.loginPage errorMsg)

routes storage { method: Post, path: [ "auth", "login" ], body } = do
  liftEffect $ log "Processing login"
  bodyText <- toString body
  case parseLoginForm bodyText of
    Just { username, password } -> do
      authResult <- storage.authenticateUser username password
      case authResult of
        Right user -> do
          liftEffect $ log $ "User authenticated: " <> show user.username
          -- Set a simple session cookie and redirect to main page
          found'
            (header "Set-Cookie" "session=authenticated; Path=/; HttpOnly" <> header "Location" "/")
            "/"
        Left InvalidCredentials ->
          found' (header "Location" "/login?error=invalid_credentials") "/login?error=invalid_credentials"
        Left e -> do
          liftEffect $ log $ show e
          found'
            (header "Location" "/login?error=server_error")
            "/login?error=server_error"
    Nothing ->
      found' (header "Location" "/login?error=invalid_data") "/login?error=invalid_data"

routes _ { method: Post, path: [ "auth", "logout" ] } = do
  liftEffect $ log "Processing logout"
  found'
    (header "Set-Cookie" "session=; Path=/; HttpOnly; Expires=Thu, 01 Jan 1970 00:00:00 GMT" <> header "Location" "/login")
    "/login"

-- Protected routes
routes storage request@{ method: Get, path: [] } = requireAuth request do
  liftEffect $ log "Serving full cleaning schedule"
  schedule <- fetchCleaningSchedule storage
  ok' (header "Content-Type" "text/html") (Html.cleaningSchedulePage schedule)

routes storage request@{ method: Get, path: [ "apartment", apartmentName ] } = requireAuth request do
  liftEffect $ log $ "Serving apartment page for: " <> apartmentName
  schedule <- fetchCleaningSchedule storage
  case normalizeApartmentName apartmentName of
    Just apartment ->
      case Map.lookup apartment schedule of
        Just windows ->
          ok' (header "Content-Type" "text/html") (Html.apartmentPage apartment windows)
        Nothing ->
          notFound
    Nothing ->
      notFound

routes _ { method: Get, path: [ "tailwind.css" ] } = do
  liftEffect $ log "Serving Tailwind CSS"
  result <- attempt $ readTextFile UTF8 "./dist/tailwind.css"
  case result of
    Right css ->
      ok' (header "Content-Type" "text/css") css
    Left _ ->
      notFound

routes _ { method: Get, path: [ "application.js" ] } = do
  liftEffect $ log "Serving JavaScript application"
  result <- attempt $ readTextFile UTF8 "./dist/application.js"
  case result of
    Right js ->
      ok' (header "Content-Type" "application/javascript") js
    Left _ ->
      notFound

routes storage request@{ method: Post, path: [ "apartments", apartmentName, "time-blocks", dateStr, timeOfDayStr ] } = requireAuth request do
  liftEffect $ log $ "Enabling time block: " <> apartmentName <> " " <> dateStr <> " " <> timeOfDayStr
  case parsePathParameters apartmentName dateStr timeOfDayStr of
    Just { apartment, date, timeOfDay } -> do
      let timeBlock = createTimeBlock apartment date timeOfDay true
      _ <- attempt $ storage.enableTimeBlock timeBlock

      -- Return the updated frame content
      schedule <- fetchCleaningSchedule storage
      case findCleaningWindowByTimeBlock timeBlock schedule of
        Just window ->
          ok' (header "Content-Type" "text/html") (Html.cleaningWindowCard { isFirst: false, isOpen: true } window)
        Nothing ->
          notFound
    Nothing ->
      notFound

routes storage request@{ method: Delete, path: [ "apartments", apartmentName, "time-blocks", dateStr, timeOfDayStr ] } = requireAuth request do
  liftEffect $ log $ "Disabling time block: " <> apartmentName <> " " <> dateStr <> " " <> timeOfDayStr
  case parsePathParameters apartmentName dateStr timeOfDayStr of
    Just { apartment, date, timeOfDay } -> do
      let timeBlock = createTimeBlock apartment date timeOfDay false
      _ <- attempt $ storage.disableTimeBlock timeBlock

      -- Return the updated frame content
      schedule <- fetchCleaningSchedule storage
      case findCleaningWindowByTimeBlock timeBlock schedule of
        Just window ->
          ok' (header "Content-Type" "text/html") (Html.cleaningWindowCard { isFirst: false, isOpen: true } window)
        Nothing ->
          notFound
    Nothing ->
      notFound

routes _ _ = do
  liftEffect $ log "404 - Page not found"
  notFound

fetchCleaningSchedule :: Storage -> Aff (Map.Map Apartment (Array Cleaning.CleaningWindow))
fetchCleaningSchedule storage = do
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
  let baseSchedule = Cleaning.scheduleFromGuestStaysWithDate currentDate guestStays

  -- Update each CleaningWindow with disabled TimeBlocks from storage
  traverse (traverse (updateCleaningWindow storage)) baseSchedule
  where

  updateCleaningWindow :: Storage -> Cleaning.CleaningWindow -> Aff Cleaning.CleaningWindow
  updateCleaningWindow st window = do
    disabledBlocks <- st.disabledTimeBlocksDuringStay window
    liftEffect $ log $ "Disabled blocks found: " <> show (Array.length disabledBlocks)
    liftEffect $ log $ "Disabled blocks: " <> show disabledBlocks
    pure $ mergeDisabledTimeBlocks window disabledBlocks

  mergeDisabledTimeBlocks :: Cleaning.CleaningWindow -> Array Cleaning.TimeBlock -> Cleaning.CleaningWindow
  mergeDisabledTimeBlocks (Cleaning.CleaningWindow record) disabledBlocks =
    let
      updatedTimeBlocks = map (updateTimeBlockAvailability disabledBlocks) record.timeBlocks
    in
      Cleaning.CleaningWindow record { timeBlocks = updatedTimeBlocks }

  updateTimeBlockAvailability :: Array Cleaning.TimeBlock -> Cleaning.TimeBlock -> Cleaning.TimeBlock
  updateTimeBlockAvailability disabledBlocks timeBlock@(Cleaning.TimeBlock tb) =
    let
      isDisabled = Array.any (timeBlockMatches timeBlock) disabledBlocks
      newAvailable = if isDisabled then false else tb.available
    in
      Cleaning.TimeBlock tb { available = newAvailable }

  timeBlockMatches :: Cleaning.TimeBlock -> Cleaning.TimeBlock -> Boolean
  timeBlockMatches (Cleaning.TimeBlock a) (Cleaning.TimeBlock b) =
    a.date == b.date && a.timeOfDay == b.timeOfDay && a.apartment == b.apartment

startServer :: { storage :: Storage, port :: Int } -> ServerM
startServer { port, storage } = do
  log $ "🚀 Airbnbeast web server starting on port " <> show port
  log "📋 Available routes:"
  log "  / - Full cleaning schedule"
  log "  /apartment/:name - Apartment-specific schedule"
  log "  POST /apartments/:apartment/time-blocks/:date/:timeOfDay - Enable time block"
  log "  DELETE /apartments/:apartment/time-blocks/:date/:timeOfDay - Disable time block"
  log ""
  serve port (routes storage) do
    log $ "✅ Server is running on http://localhost:" <> show port