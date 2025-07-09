module Airbnbeast.WebServer where

import Prelude

import Airbnbeast.Auth (AuthError(..), Session, User)
import Airbnbeast.Availability (Apartment(..), GuestStay, ReservationSource(..), fetchGuestStays)
import Airbnbeast.Cleaning (TimeOfDay(..), TimeBlock(..))
import Airbnbeast.Cleaning as Cleaning
import Airbnbeast.Config (AirbnbeastConfig)
import Airbnbeast.Html as Html
import Airbnbeast.I18n as I18n
import Airbnbeast.Session (SessionConfig, createSession, createSessionCookie, defaultSessionConfig, parseSessionFromCookie, validateSession)
import Airbnbeast.Storage (Storage)
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Date (Date)
import Data.Date as Date
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (nowDate)
import Foreign.Object as Object
import HTTPure (Request, ResponseM, ServerM, Response, found', header, notFound, ok', serve, response)
import HTTPure.Body (toString)
import HTTPure.Method (Method(..))
import JSURI (decodeURIComponent)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)

type Routes = Request -> ResponseM

-- | Application context containing current user and storage
type Context =
  { currentUser :: Maybe User
  , request :: Request
  , storage :: Storage
  , sessionConfig :: SessionConfig
  }

-- | Application monad that provides access to context
type AppM = ReaderT Context Aff

-- | New context-aware route handler type
type RouteHandler = AppM Response

-- | Convert an AppM computation to a ResponseM for HTTPure compatibility
runRoute :: AirbnbeastConfig -> Request -> AppM Response -> ResponseM
runRoute { sessionConfig, storage } request appAction = do
  let
    currentUser = getSessionFromRequest sessionConfig request >>= \session -> Just session.user
    context = { currentUser, request, storage, sessionConfig }
  runReaderT appAction context

-- | Helper to get the current user from context
getCurrentUser :: AppM (Maybe User)
getCurrentUser = asks _.currentUser

-- | Helper to get storage from context
getStorage :: AppM Storage
getStorage = asks _.storage

-- | Helper to create a redirect response to login
redirectToLogin :: AppM Response
redirectToLogin = liftAff $ found' (header "Location" "/login") "/login"

-- | Helper to create a forbidden response
forbidden :: AppM Response
forbidden = liftAff $ response 403 "Forbidden"

-- Secure session management with HMAC signatures
getSessionFromRequest :: SessionConfig -> Request -> Maybe Session
getSessionFromRequest sessionConfig request = do
  let headersStr = show request.headers
  case extractCookieHeader headersStr of
    Nothing -> Nothing -- No cookie header found
    Just cookieHeader -> do
      case parseSessionFromCookie cookieHeader of
        Nothing -> Nothing -- No session token in cookies
        Just sessionToken -> do
          -- Log session validation attempt for debugging
          case validateSession sessionConfig sessionToken of
            Just session -> Just session
            Nothing -> Nothing

-- Extract cookie header from headers string representation
extractCookieHeader :: String -> Maybe String
extractCookieHeader headersStr = do
  let lines = String.split (String.Pattern "\n") headersStr
  -- Try both lowercase and capitalized versions
  cookieLine <- Array.find
    ( \line ->
        String.contains (String.Pattern "cookie:") (String.toLower line) ||
          String.contains (String.Pattern "Cookie:") line
    )
    lines
  let cleanLine = String.trim cookieLine
  case String.stripPrefix (String.Pattern "cookie:") (String.toLower cleanLine) of
    Just value -> Just $ String.trim value
    Nothing -> case String.stripPrefix (String.Pattern "Cookie:") cleanLine of
      Just value -> Just $ String.trim value
      Nothing -> Nothing

requireAuth :: (User -> AppM Response) -> AppM Response
requireAuth action = do
  maybeUser <- refreshUser
  case maybeUser of
    Just user -> action user
    Nothing -> redirectToLogin

  where
  refreshUser :: AppM (Maybe User)
  refreshUser = do
    storage <- getStorage
    currentUser <- getCurrentUser

    case currentUser of
      Just user -> do
        lift $ storage.fetchUserById user.id

      Nothing ->
        pure Nothing

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
  encodedValue <- String.stripPrefix (String.Pattern keyPrefix) pair
  pure $ case decodeURIComponent encodedValue of
    Just decoded -> decoded
    Nothing -> encodedValue -- fallback to original if decoding fails

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

type GuestStayFormData =
  { apartment :: String
  , fromDate :: String
  , toDate :: String
  , last4Digits :: String
  , link :: String
  }

parseGuestStayForm :: String -> Maybe GuestStayFormData
parseGuestStayForm formData = do
  let pairs = String.split (String.Pattern "&") formData
  apartment <- getFormValue "apartment" pairs
  fromDate <- getFormValue "fromDate" pairs
  toDate <- getFormValue "toDate" pairs
  last4Digits <- getFormValue "last4Digits" pairs
  link <- getFormValue "link" pairs
  pure { apartment, fromDate, toDate, last4Digits, link }

createInternalGuestStay :: GuestStayFormData -> AppM GuestStay
createInternalGuestStay { apartment, fromDate, toDate, last4Digits, link } = do
  let
    fallbackDate = unsafePartial $ fromMaybe bottom $ Date.canonicalDate <$> toEnum 2024 <*> toEnum 1 <*> toEnum 1
    parsedFromDate = fromMaybe fallbackDate $ parseDate fromDate
    parsedToDate = fromMaybe fallbackDate $ parseDate toDate

  uuid <- liftEffect UUID.genUUID

  pure
    { id: UUID.toString uuid
    , apartment: Apartment apartment
    , fromDate: parsedFromDate
    , toDate: parsedToDate
    , last4Digits: last4Digits
    , link: link
    , source: Internal
    }

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

-- | ReaderT-based home handler
homeHandler :: RouteHandler
homeHandler = requireAuth \user -> do
  liftEffect $ log $ "🏠 Serving full cleaning schedule for user: " <> show user.username
  storage <- getStorage
  schedule <- liftAff $ fetchCleaningSchedule storage
  liftAff $ ok' (header "Content-Type" "text/html") (Html.cleaningSchedulePage user.isAdmin schedule)

-- | Index page handler with admin-aware navigation
indexHandler :: RouteHandler
indexHandler = requireAuth \user -> do
  liftEffect $ log $ "🏠 Serving index page for user: " <> show user.username
  lift $ ok' (header "Content-Type" "text/html") (Html.indexPage user.isAdmin)

-- | ReaderT-based login handler
loginHandler :: RouteHandler
loginHandler = do
  query <- asks _.request.query
  liftEffect $ log "📝 Serving login page"
  let
    errorMsg = case Object.lookup "error" query of
      Just "invalid_credentials" -> Just I18n.pt.invalidCredentials
      Just "server_error" -> Just I18n.pt.serverError
      Just "invalid_data" -> Just I18n.pt.invalidLoginData
      _ -> Nothing
  liftAff $ ok' (header "Content-Type" "text/html") (Html.loginPage errorMsg)

loginAttemptHandler :: RouteHandler
loginAttemptHandler = do
  { body } <- asks _.request
  storage <- getStorage
  sessionConfig <- asks _.sessionConfig

  liftEffect $ log "Processing login"
  bodyText <- liftAff $ toString body
  case parseLoginForm bodyText of
    Just { username, password } -> do
      authResult <- lift $ storage.authenticateUser username password
      case authResult of
        Right user -> do
          liftEffect $ log $ "User authenticated: " <> show user.username
          -- Create secure session token
          sessionToken <- lift $ createSession sessionConfig user
          liftEffect $ log $ "🔑 Created session token: " <> String.take 20 sessionToken <> "..."
          liftEffect $ log $ "🍪 Setting cookie: " <> createSessionCookie sessionToken

          -- Redirect based on user role
          let redirectUrl = if user.isAdmin then "/" else "/schedule"
          liftEffect $ log $ "Redirecting " <> (if user.isAdmin then "admin" else "non-admin") <> " user to: " <> redirectUrl

          found'
            (header "Set-Cookie" (createSessionCookie sessionToken) <> header "Location" redirectUrl)
            redirectUrl
        Left InvalidCredentials ->
          found' (header "Location" "/login?error=invalid_credentials") "/login?error=invalid_credentials"
        Left e -> do
          liftEffect $ log $ show e
          found'
            (header "Location" "/login?error=server_error")
            "/login?error=server_error"
    Nothing ->
      found' (header "Location" "/login?error=invalid_data") "/login?error=invalid_data"

logoutHandler :: RouteHandler
logoutHandler = do
  liftEffect $ log "Processing logout"
  found'
    (header "Set-Cookie" "_airbnbeast_session=; Path=/; HttpOnly; Expires=Thu, 01 Jan 1970 00:00:00 GMT" <> header "Location" "/login")
    "/login"

apartmentHandler :: String -> RouteHandler
apartmentHandler apartmentName = requireAuth \user -> do
  liftEffect $ log $ "Serving apartment page for: " <> apartmentName
  storage <- getStorage
  schedule <- lift $ fetchCleaningSchedule storage

  case normalizeApartmentName apartmentName of
    Just apartment ->
      case Map.lookup apartment schedule of
        Just windows ->
          ok' (header "Content-Type" "text/html") (Html.apartmentPage user.isAdmin apartment windows)
        Nothing ->
          notFound
    Nothing ->
      notFound

type TimeBlockIdentifier =
  { apartmentName :: String
  , dateStr :: String
  , timeOfDayStr :: String
  }

createTimeBlockHandler :: TimeBlockIdentifier -> AppM Response
createTimeBlockHandler { apartmentName, dateStr, timeOfDayStr } = requireAuth \user -> do
  liftEffect $ log $ "Enabling time block: " <> apartmentName <> " " <> dateStr <> " " <> timeOfDayStr

  -- Check if user is admin
  if not user.isAdmin then
    forbidden
  else
    case parsePathParameters apartmentName dateStr timeOfDayStr of
      Just { apartment, date, timeOfDay } -> do
        storage <- getStorage
        let timeBlock = createTimeBlock apartment date timeOfDay true
        _ <- lift $ attempt $ storage.enableTimeBlock timeBlock

        -- Return the updated frame content
        schedule <- lift $ fetchCleaningSchedule storage
        case findCleaningWindowByTimeBlock timeBlock schedule of
          Just window ->
            ok' (header "Content-Type" "text/html") (Html.cleaningWindowCard { isFirst: false, isOpen: true, isAdmin: user.isAdmin } window)
          Nothing ->
            notFound
      Nothing ->
        notFound

removeTimeBlockHandler :: TimeBlockIdentifier -> AppM Response
removeTimeBlockHandler { apartmentName, dateStr, timeOfDayStr } = requireAuth \user -> do
  liftEffect $ log $ "Disabling time block: " <> apartmentName <> " " <> dateStr <> " " <> timeOfDayStr

  -- Check if user is admin
  if not user.isAdmin then
    forbidden
  else
    case parsePathParameters apartmentName dateStr timeOfDayStr of
      Just { apartment, date, timeOfDay } -> do
        let timeBlock = createTimeBlock apartment date timeOfDay false
        storage <- getStorage
        _ <- lift $ attempt $ storage.disableTimeBlock timeBlock

        -- Return the updated frame content
        schedule <- lift $ fetchCleaningSchedule storage
        case findCleaningWindowByTimeBlock timeBlock schedule of
          Just window ->
            ok' (header "Content-Type" "text/html") (Html.cleaningWindowCard { isFirst: false, isOpen: true, isAdmin: user.isAdmin } window)
          Nothing ->
            notFound
      Nothing ->
        notFound

staticFileHandler :: String -> String -> AppM Response
staticFileHandler contentType file = do
  result <- lift $ attempt $ readTextFile UTF8 ("./dist/" <> file)

  case result of
    Right contents ->
      ok' (header "Content-Type" contentType) contents
    Left _ ->
      notFound

-- Admin-only GuestStay management handlers
guestStaysListHandler :: RouteHandler
guestStaysListHandler = requireAuth \user -> do
  if not user.isAdmin then
    forbidden
  else do
    query <- asks _.request.query
    liftEffect $ log "Serving guest stays management page"
    storage <- getStorage

    -- Fetch Internal guest stays for all apartments
    gloriaStays <- lift $ storage.fetchStoredGuestStays (Apartment "Glória")
    santaStays <- lift $ storage.fetchStoredGuestStays (Apartment "Santa")
    let allStays = gloriaStays <> santaStays

    let
      errorMsg = case Object.lookup "error" query of
        Just "delete_failed" -> Just "Failed to delete guest stay. Please try again."
        Just "not_found" -> Just "Guest stay not found."
        _ -> Nothing

    lift $ ok' (header "Content-Type" "text/html") (Html.guestStaysListPageWithError user.isAdmin allStays errorMsg)

newGuestStayFormHandler :: RouteHandler
newGuestStayFormHandler = requireAuth \user -> do
  if not user.isAdmin then
    forbidden
  else do
    liftEffect $ log "Serving new guest stay form"
    lift $ ok' (header "Content-Type" "text/html") Html.newGuestStayFormPage

createGuestStayHandler :: RouteHandler
createGuestStayHandler = requireAuth \user -> do
  if not user.isAdmin then
    forbidden
  else do
    { body } <- asks _.request
    storage <- getStorage

    liftEffect $ log "Creating new guest stay"
    bodyText <- liftAff $ toString body

    case parseGuestStayForm bodyText of
      Just guestStayData -> do
        guestStay <- createInternalGuestStay guestStayData
        _ <- lift $ storage.saveGuestStay guestStay

        lift $ found' (header "Location" "/admin/guest-stays") "/admin/guest-stays"
      Nothing ->
        lift $ found' (header "Location" "/admin/guest-stays/new?error=invalid_data") "/admin/guest-stays/new?error=invalid_data"

editGuestStayFormHandler :: String -> RouteHandler
editGuestStayFormHandler guestStayId = requireAuth \user -> do
  if not user.isAdmin then
    forbidden
  else do
    liftEffect $ log $ "Serving edit form for guest stay: " <> guestStayId
    storage <- getStorage

    maybeGuestStay <- lift $ storage.fetchGuestStayById guestStayId
    case maybeGuestStay of
      Just guestStay ->
        lift $ ok' (header "Content-Type" "text/html") (Html.editGuestStayFormPage guestStay)
      Nothing ->
        lift $ found' (header "Location" "/admin/guest-stays?error=not_found") "/admin/guest-stays?error=not_found"

updateGuestStayHandler :: String -> RouteHandler
updateGuestStayHandler guestStayId = requireAuth \user -> do
  if not user.isAdmin then
    forbidden
  else do
    { body } <- asks _.request
    storage <- getStorage

    liftEffect $ log $ "Updating guest stay: " <> guestStayId
    bodyText <- liftAff $ toString body

    case parseGuestStayForm bodyText of
      Just guestStayData -> do
        -- Create updated guest stay with the same ID but new data
        let
          fallbackDate = unsafePartial $ fromMaybe bottom $ Date.canonicalDate <$> toEnum 2024 <*> toEnum 1 <*> toEnum 1
          parsedFromDate = fromMaybe fallbackDate $ parseDate guestStayData.fromDate
          parsedToDate = fromMaybe fallbackDate $ parseDate guestStayData.toDate

          updatedGuestStay =
            { id: guestStayId -- Keep the same ID
            , apartment: Apartment guestStayData.apartment
            , fromDate: parsedFromDate
            , toDate: parsedToDate
            , last4Digits: guestStayData.last4Digits
            , link: guestStayData.link
            , source: Internal
            }

        _ <- lift $ storage.saveGuestStay updatedGuestStay
        lift $ found' (header "Location" "/admin/guest-stays") "/admin/guest-stays"
      Nothing ->
        lift $ found' (header "Location" ("/admin/guest-stays/" <> guestStayId <> "/edit?error=invalid_data")) ("/admin/guest-stays/" <> guestStayId <> "/edit?error=invalid_data")

deleteGuestStayHandler :: String -> RouteHandler
deleteGuestStayHandler guestStayId = requireAuth \user -> do
  if not user.isAdmin then
    forbidden
  else do
    storage <- getStorage
    liftEffect $ log $ "Deleting guest stay: " <> guestStayId

    result <- lift $ attempt $ storage.deleteGuestStay guestStayId
    case result of
      Right _ ->
        lift $ found' (header "Location" "/admin/guest-stays" <> header "Turbo-Method" "get") "/admin/guest-stays"
      Left error -> do
        liftEffect $ log $ "Failed to delete guest stay: " <> show error
        lift $ found' (header "Location" "/admin/guest-stays?error=delete_failed" <> header "Turbo-Method" "get") "/admin/guest-stays?error=delete_failed"

routes :: AirbnbeastConfig -> Routes
-- Login routes (migrated to ReaderT)
routes config request = runRoute config request $
  case request of
    { method: Get, path: [ "login" ] } ->
      loginHandler

    { method: Post, path: [ "auth", "login" ] } ->
      loginAttemptHandler

    { method: Post, path: [ "auth", "logout" ] } ->
      logoutHandler

    { method: Get, path: [] } ->
      indexHandler

    { method: Get, path: [ "schedule" ] } ->
      homeHandler

    { method: Get, path: [ "apartment", apartmentName ] } ->
      apartmentHandler apartmentName

    { method: Get, path: [ "tailwind.css" ] } ->
      staticFileHandler "text/css" "tailwind.css"

    { method: Get, path: [ "application.js" ] } ->
      staticFileHandler "application/javascript" "application.js"

    { method: Post, path: [ "apartments", apartmentName, "time-blocks", dateStr, timeOfDayStr ] } ->
      createTimeBlockHandler { apartmentName, dateStr, timeOfDayStr }

    { method: Delete, path: [ "apartments", apartmentName, "time-blocks", dateStr, timeOfDayStr ] } ->
      removeTimeBlockHandler { apartmentName, dateStr, timeOfDayStr }

    { method: Get, path: [ "admin", "guest-stays" ] } ->
      guestStaysListHandler

    { method: Get, path: [ "admin", "guest-stays", "new" ] } ->
      newGuestStayFormHandler

    { method: Post, path: [ "admin", "guest-stays" ] } ->
      createGuestStayHandler

    { method: Get, path: [ "admin", "guest-stays", guestStayId, "edit" ] } ->
      editGuestStayFormHandler guestStayId

    { method: Put, path: [ "admin", "guest-stays", guestStayId ] } ->
      updateGuestStayHandler guestStayId

    { method: Post, path: [ "admin", "guest-stays", guestStayId ] } ->
      updateGuestStayHandler guestStayId

    { method: Delete, path: [ "admin", "guest-stays", guestStayId ] } ->
      deleteGuestStayHandler guestStayId

    _ -> do
      liftEffect $ log "404 - Page not found"
      notFound

fetchCleaningSchedule :: Storage -> Aff (Map.Map Apartment (Array Cleaning.CleaningWindow))
fetchCleaningSchedule storage = do
  let
    apartments =
      [ Apartment "Glória"
      , Apartment "Santa"
      ]
    airbnbCalendars =
      [ { apartment: Apartment "Glória"
        , icsUrl: "https://www.airbnb.com/calendar/ical/47420131.ics?s=317a8c2653c6a64a4cc4ed1ad89f6afd"
        }
      , { apartment: Apartment "Santa"
        , icsUrl: "https://www.airbnb.com/calendar/ical/568955469596249266.ics?s=878a19388f719853be173bf4ad3dd77c"
        }
      ]

  currentDate <- liftEffect nowDate

  -- Fetch Airbnb-sourced GuestStays
  airbnbGuestStays <- fetchGuestStays airbnbCalendars

  -- Fetch database-sourced GuestStays for each apartment
  storedGuestStays <- traverse
    ( \apartment -> do
        stays <- storage.fetchStoredGuestStays apartment
        pure (apartment /\ stays)
    )
    apartments
  let storedGuestStaysMap = Map.fromFoldable storedGuestStays

  -- Mix Airbnb and stored guest stays, then sort by fromDate
  let
    mixedGuestStays = Map.unionWith
      (\airbnbStays dbStays -> Array.sortBy (\a b -> compare a.fromDate b.fromDate) (airbnbStays <> dbStays))
      airbnbGuestStays
      storedGuestStaysMap

  -- Generate cleaning schedule from mixed guest stays
  let baseSchedule = Cleaning.scheduleFromGuestStaysWithDate currentDate mixedGuestStays

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

startServer :: { sessionConfig :: SessionConfig, storage :: Storage, port :: Int } -> ServerM
startServer { port, storage, sessionConfig } = do
  log $ "🚀 Airbnbeast web server starting on port " <> show port
  log "📋 Available routes:"
  log "  / - Full cleaning schedule"
  log "  /apartment/:name - Apartment-specific schedule"
  log "  POST /apartments/:apartment/time-blocks/:date/:timeOfDay - Enable time block"
  log "  DELETE /apartments/:apartment/time-blocks/:date/:timeOfDay - Disable time block"
  log ""
  serve port (routes { sessionConfig, storage }) do
    log $ "✅ Server is running on http://localhost:" <> show port