module Airbnbeast.WebServer.ReaderT where

import Prelude

import Airbnbeast.Availability (Apartment(..), fetchGuestStays)
import Airbnbeast.Cleaning as Cleaning
import Airbnbeast.Cleaning (TimeOfDay(..), TimeBlock(..))
import Airbnbeast.Html as Html
import Airbnbeast.Storage (Storage)
import Airbnbeast.Auth (Session, User)
import Airbnbeast.Session (defaultSessionConfig, validateSession, parseSessionFromCookie)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Array as Array
import Data.Date (Date)
import Data.Date as Date
import Data.Enum (toEnum)
import Data.Int as Int
import Data.Traversable (traverse)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Aff.Class (liftAff)
import Effect.Console (log)
import Effect.Now (nowDate, nowDateTime)
import HTTPure (Request, Response, ResponseM, header, notFound, ok', found')
import Unsafe.Coerce (unsafeCoerce)

-- | Application context containing current user and storage
type Context = 
  { currentUser :: Maybe User
  , storage :: Storage
  }

-- | Application monad that provides access to context
type AppM = ReaderT Context Aff

-- | Run an AppM computation with the given context
runAppM :: forall a. Context -> AppM a -> Aff a
runAppM context computation = runReaderT computation context

-- | New context-aware route handler type
type RouteHandler = Request -> AppM Response

-- | Convert an AppM computation to a ResponseM for HTTPure compatibility
runRoute :: Storage -> Request -> AppM Response -> ResponseM
runRoute storage request appAction = do
  let 
    currentUser = getSessionFromRequest request >>= \session -> Just session.user
    context = { currentUser, storage }
  runAppM context appAction

-- | Helper to get the current user from context
getCurrentUser :: AppM (Maybe User)
getCurrentUser = asks _.currentUser

-- | Helper to get storage from context
getStorage :: AppM Storage
getStorage = asks _.storage

-- | Helper to create a redirect response to login
redirectToLogin :: AppM Response
redirectToLogin = liftAff $ found' (header "Location" "/login") "/login"

-- | Helper that runs an action if authenticated, otherwise redirects to login
requireAuth :: (User -> AppM Response) -> AppM Response
requireAuth action = do
  maybeUser <- getCurrentUser
  case maybeUser of
    Just user -> action user
    Nothing -> redirectToLogin

-- | Extract session from request headers (same logic as main WebServer)
getSessionFromRequest :: Request -> Maybe Session
getSessionFromRequest request = do
  let headersStr = show request.headers
  case extractCookieHeader headersStr of
    Nothing -> Nothing
    Just cookieHeader -> do
      case parseSessionFromCookie cookieHeader of
        Nothing -> Nothing
        Just sessionToken -> validateSession defaultSessionConfig sessionToken

-- | Extract cookie header from headers string representation
extractCookieHeader :: String -> Maybe String
extractCookieHeader headersStr = do
  let lines = String.split (String.Pattern "\n") headersStr
  cookieLine <- Array.find (\line -> 
    String.contains (String.Pattern "cookie:") (String.toLower line) ||
    String.contains (String.Pattern "Cookie:") line) lines
  let cleanLine = String.trim cookieLine
  case String.stripPrefix (String.Pattern "cookie:") (String.toLower cleanLine) of
    Just value -> Just $ String.trim value
    Nothing -> case String.stripPrefix (String.Pattern "Cookie:") cleanLine of
      Just value -> Just $ String.trim value
      Nothing -> Nothing

-- | Example ReaderT-based route handlers

-- | Home page handler with ReaderT
homeHandler :: RouteHandler  
homeHandler _ = requireAuth \user -> do
  liftEffect $ log $ "🏠 Serving full cleaning schedule for user: " <> show user.username
  storage <- getStorage
  schedule <- fetchCleaningSchedule storage
  liftAff $ ok' (header "Content-Type" "text/html") (Html.cleaningSchedulePage schedule)

-- | Apartment page handler with ReaderT
apartmentHandler :: String -> RouteHandler
apartmentHandler apartmentName _ = requireAuth \user -> do
  liftEffect $ log $ "🏢 Serving apartment " <> apartmentName <> " for user: " <> show user.username
  storage <- getStorage
  schedule <- fetchCleaningSchedule storage
  case normalizeApartmentName apartmentName of
    Just apartment ->
      case Map.lookup apartment schedule of
        Just windows ->
          liftAff $ ok' (header "Content-Type" "text/html") (Html.apartmentPage apartment windows)
        Nothing ->
          liftAff notFound
    Nothing ->
      liftAff notFound

-- | Enable time block handler with ReaderT
enableTimeBlockHandler :: String -> String -> String -> RouteHandler
enableTimeBlockHandler apartmentName dateStr timeOfDayStr _ = requireAuth \user -> do
  liftEffect $ log $ "✅ User " <> show user.username <> " enabling time block: " <> apartmentName <> " " <> dateStr <> " " <> timeOfDayStr
  storage <- getStorage
  case parsePathParameters apartmentName dateStr timeOfDayStr of
    Just { apartment, date, timeOfDay } -> do
      let timeBlock = createTimeBlock apartment date timeOfDay true
      _ <- liftAff $ storage.enableTimeBlock timeBlock

      -- Return the updated frame content
      schedule <- fetchCleaningSchedule storage
      case findCleaningWindowByTimeBlock timeBlock schedule of
        Just window ->
          liftAff $ ok' (header "Content-Type" "text/html") (Html.cleaningWindowCard { isFirst: false, isOpen: true } window)
        Nothing ->
          liftAff notFound
    Nothing ->
      liftAff notFound

-- | Example of how to integrate with existing WebServer
-- This shows how you could gradually migrate routes to use ReaderT

{-
-- In your main WebServer module, you could add:

routes storage request@{ method: Get, path: [] } = 
  runRoute storage request (homeHandler request)

routes storage request@{ method: Get, path: [ "apartment", apartmentName ] } = 
  runRoute storage request (apartmentHandler apartmentName request)

routes storage request@{ method: Post, path: [ "apartments", apartmentName, "time-blocks", dateStr, timeOfDayStr ] } = 
  runRoute storage request (enableTimeBlockHandler apartmentName dateStr timeOfDayStr request)
-}

-- Helper functions (would normally be imported from main WebServer)

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

-- Import and implement helper functions from main WebServer

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
parseTimeOfDay "Morning" = Just Morning
parseTimeOfDay "Afternoon" = Just Afternoon
parseTimeOfDay _ = Nothing

createTimeBlock :: Apartment -> Date -> TimeOfDay -> Boolean -> TimeBlock
createTimeBlock apartment date timeOfDay available =
  TimeBlock { date, timeOfDay, available, apartment }

findCleaningWindowByTimeBlock :: TimeBlock -> Map.Map Apartment (Array Cleaning.CleaningWindow) -> Maybe Cleaning.CleaningWindow
findCleaningWindowByTimeBlock (TimeBlock { apartment }) schedule = do
  windows <- Map.lookup apartment schedule
  Array.head windows -- Simplified for demo

fetchCleaningSchedule :: Storage -> AppM (Map.Map Apartment (Array Cleaning.CleaningWindow))
fetchCleaningSchedule storage = do
  liftEffect $ log "📅 Fetching cleaning schedule with ReaderT context"
  -- This would be the actual implementation from WebServer
  let
    gloria = { apartment: Apartment "Glória", icsUrl: "https://example.com" }
    santa = { apartment: Apartment "Santa", icsUrl: "https://example.com" }

  currentDate <- liftEffect nowDate
  -- For demo purposes, return empty schedule
  pure Map.empty