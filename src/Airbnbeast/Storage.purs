module Airbnbeast.Storage
  ( Storage
  , sqliteEnableTimeBlock
  , sqliteStorage
  ) where

import Prelude

import Airbnbeast.Availability (Apartment(..), GuestStay, ReservationSource(..))
import Airbnbeast.Cleaning (CleaningWindow(..), TimeBlock(..), TimeOfDay(..))
import Airbnbeast.Auth (User, UserId(..), Username(..), AuthError(..))
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Date as Date
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign (unsafeFromForeign, unsafeToForeign)
import Node.Bcrypt (comparePassword)
import SQLite3 as SQLite3

type Storage =
  { disableTimeBlock :: TimeBlock -> Aff TimeBlock
  , enableTimeBlock :: TimeBlock -> Aff TimeBlock
  , disabledTimeBlocksDuringStay :: CleaningWindow -> Aff (Array TimeBlock)
  , authenticateUser :: String -> String -> Aff (Either AuthError User)
  , fetchUserById :: UserId -> Aff (Maybe User)
  , saveGuestStay :: GuestStay -> Aff GuestStay
  , deleteGuestStay :: String -> Aff Boolean
  , fetchStoredGuestStays :: Apartment -> Aff (Array GuestStay)
  , fetchGuestStayById :: String -> Aff (Maybe GuestStay)
  }

newtype RawTimeBlock = RawTimeBlock
  { apartment :: String
  , date :: String
  , timeOfDay :: String
  }

instance DecodeJson RawTimeBlock where
  decodeJson json = do
    obj <- decodeJson json
    apartment <- obj .: "apartment"
    timeOfDay <- obj .: "time_of_day"
    date <- obj .: "date"

    pure $ RawTimeBlock { apartment, date, timeOfDay }

newtype RawUser = RawUser
  { id :: Int
  , username :: String
  , password_hash :: String
  , is_admin :: Boolean
  }

instance DecodeJson RawUser where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    username <- obj .: "username"
    password_hash <- obj .: "password_hash"
    is_admin <- (\b -> b == 1) <$> obj .: "is_admin"
    pure $ RawUser { id, username, password_hash, is_admin }

newtype RawGuestStay = RawGuestStay
  { id :: String
  , apartment :: String
  , from_date :: String
  , to_date :: String
  , last_4_digits :: String
  , link :: String
  , notes :: String
  }

instance DecodeJson RawGuestStay where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    apartment <- obj .: "apartment"
    from_date <- obj .: "from_date"
    to_date <- obj .: "to_date"
    last_4_digits <- obj .: "last_4_digits"
    link <- obj .: "link"
    notes <- obj .: "notes"
    pure $ RawGuestStay { id, apartment, from_date, to_date, last_4_digits, link, notes }

sqliteStorage :: SQLite3.DBConnection -> Storage
sqliteStorage conn =
  { disableTimeBlock: sqliteDisableTimeBlock conn
  , enableTimeBlock: sqliteEnableTimeBlock conn
  , disabledTimeBlocksDuringStay: sqliteDisabledTimeBlocksDuringStay conn
  , authenticateUser: sqliteAuthenticateUser conn
  , fetchUserById: sqliteFetchUserById conn
  , saveGuestStay: sqliteSaveGuestStay conn
  , deleteGuestStay: sqliteDeleteGuestStay conn
  , fetchStoredGuestStays: sqliteFetchStoredGuestStays conn
  , fetchGuestStayById: sqliteFetchGuestStayById conn
  }

sqliteDisableTimeBlock :: SQLite3.DBConnection -> TimeBlock -> Aff TimeBlock
sqliteDisableTimeBlock conn timeBlock@(TimeBlock { apartment, date, timeOfDay }) = do
  liftEffect $ Console.log $ "Disabling time block: " <> show apartment <> " " <> show date <> " " <> show timeOfDay

  let
    apartmentStr = case apartment of
      Apartment name -> name
    dateStr = formatDate date
    timeOfDayStr = show timeOfDay

    sql =
      """
      INSERT OR IGNORE INTO disabled_time_blocks (apartment, date, time_of_day)
      VALUES (?, ?, ?)
    """

  _ <- SQLite3.queryDB conn sql [ unsafeToForeign apartmentStr, unsafeToForeign dateStr, unsafeToForeign timeOfDayStr ]

  -- Return the time block with available set to false
  pure $ TimeBlock $ (unwrap timeBlock) { available = false }

sqliteEnableTimeBlock :: SQLite3.DBConnection -> TimeBlock -> Aff TimeBlock
sqliteEnableTimeBlock conn timeBlock@(TimeBlock { apartment, date, timeOfDay }) = do
  liftEffect $ Console.log $ "Enabling time block: " <> show apartment <> " " <> show date <> " " <> show timeOfDay

  let
    apartmentStr = case apartment of
      Apartment name -> name
    dateStr = formatDate date
    timeOfDayStr = show timeOfDay

    sql =
      """
      DELETE FROM disabled_time_blocks
      WHERE apartment = ? AND date = ? AND time_of_day = ?
    """

  _ <- SQLite3.queryDB conn sql [ unsafeToForeign apartmentStr, unsafeToForeign dateStr, unsafeToForeign timeOfDayStr ]

  -- Return the time block with available set to true
  pure $ TimeBlock $ (unwrap timeBlock) { available = true }

-- Helper function to format date as ISO string (YYYY-MM-DD)
formatDate :: Date.Date -> String
formatDate date =
  let
    year = fromEnum $ Date.year date
    month = fromEnum $ Date.month date
    day = fromEnum $ Date.day date
    padZero n = if n < 10 then "0" <> show n else show n
  in
    show year <> "-" <> padZero month <> "-" <> padZero day

sqliteDisabledTimeBlocksDuringStay :: SQLite3.DBConnection -> CleaningWindow -> Aff (Array TimeBlock)
sqliteDisabledTimeBlocksDuringStay conn (CleaningWindow { from, to, stay }) = do
  let
    startDate = DateTime.date from
    endDate = DateTime.date to
    apartment = stay.apartment
    apartmentStr = case apartment of
      Apartment name -> name

    sql =
      """
      SELECT apartment, date, time_of_day
      FROM disabled_time_blocks
      WHERE apartment = ? AND date >= ? AND date <= ?
      ORDER BY date ASC,
               CASE time_of_day
                 WHEN 'Morning' THEN 1
                 WHEN 'Afternoon' THEN 2
                 ELSE 3
               END ASC
    """

  results <- (decodeJson <<< unsafeFromForeign) <$> SQLite3.queryDB conn sql
    [ unsafeToForeign apartmentStr
    , unsafeToForeign (formatDate startDate)
    , unsafeToForeign (formatDate endDate)
    ]

  case results of
    Right (rawTimeBlocks :: Array RawTimeBlock) -> do
      timeBlocks <- traverse convertRawTimeBlock rawTimeBlocks
      pure timeBlocks

    Left e ->
      throwError (error $ "Failed to deserialized records correctly: " <> show e)
  where
  convertRawTimeBlock :: RawTimeBlock -> Aff TimeBlock
  convertRawTimeBlock (RawTimeBlock { apartment: apartmentStr, date: dateStr, timeOfDay: timeOfDayStr }) = do
    parsedDate <- parseDate dateStr
    parsedTimeOfDay <- parseTimeOfDay timeOfDayStr
    pure $ TimeBlock
      { date: parsedDate
      , timeOfDay: parsedTimeOfDay
      , available: false -- These are disabled time blocks
      , apartment: Apartment apartmentStr
      }

  parseDate :: String -> Aff Date.Date
  parseDate dateStr = do
    case traverse Int.fromString (String.split (Pattern "-") dateStr) of
      Just [ yearStr, monthStr, dayStr ] ->
        Maybe.fromMaybe (throwError $ error $ "Invalid date format: " <> dateStr) do
          year <- toEnum yearStr
          month <- toEnum monthStr
          day <- toEnum dayStr

          pure (pure $ Date.canonicalDate year month day)

      _ -> throwError (error $ "Invalid date format: " <> dateStr)

  parseTimeOfDay :: String -> Aff TimeOfDay
  parseTimeOfDay "Morning" = pure Morning
  parseTimeOfDay "Afternoon" = pure Afternoon
  parseTimeOfDay other = throwError (error $ "Invalid time of day: " <> other)

sqliteAuthenticateUser :: SQLite3.DBConnection -> String -> String -> Aff (Either AuthError User)
sqliteAuthenticateUser conn username password = do
  liftEffect $ Console.log $ "Authenticating user: " <> username

  let
    sql =
      """
      SELECT id, username, password_hash, is_admin
      FROM users
      WHERE username = ?
      LIMIT 1
    """

  results <- (decodeJson <<< unsafeFromForeign) <$> SQLite3.queryDB conn sql
    [ unsafeToForeign username ]

  case results of
    Right (users :: Array RawUser) ->
      case users of
        [ RawUser { id, username: dbUsername, password_hash, is_admin } ] -> do
          if comparePassword password password_hash then pure $ Right
            { id: UserId id
            , username: Username dbUsername
            , isAdmin: is_admin
            }
          else pure $ Left InvalidCredentials
        [] -> pure $ Left UserNotFound
        _ -> pure $ Left (DatabaseError "Multiple users found with same username")

    Left e -> pure $ Left (DatabaseError $ "Failed to query users: " <> show e)

sqliteFetchUserById :: SQLite3.DBConnection -> UserId -> Aff (Maybe User)
sqliteFetchUserById conn (UserId userId) = do
  liftEffect $ Console.log $ "Fetching user by ID: " <> show userId

  let
    sql =
      """
      SELECT id, username, password_hash, is_admin
      FROM users
      WHERE id = ?
      LIMIT 1
    """

  results <- (decodeJson <<< unsafeFromForeign) <$> SQLite3.queryDB conn sql
    [ unsafeToForeign userId ]

  case results of
    Right (users :: Array RawUser) ->
      case users of
        [ RawUser { id, username: dbUsername, is_admin } ] ->
          pure $ Just
            { id: UserId id
            , username: Username dbUsername
            , isAdmin: is_admin
            }
        [] -> pure Nothing
        _ -> throwError (error "Multiple users found with same ID")

    Left e -> throwError (error $ "Failed to query user by ID: " <> show e)

-- GuestStay storage operations for Internal reservations
sqliteSaveGuestStay :: SQLite3.DBConnection -> GuestStay -> Aff GuestStay
sqliteSaveGuestStay conn guestStay@{ id, apartment, fromDate, toDate, last4Digits, link } = do
  liftEffect $ Console.log $ "Saving guest stay: " <> id

  let
    apartmentStr = case apartment of
      Apartment name -> name
    fromDateStr = formatDate fromDate
    toDateStr = formatDate toDate

    sql =
      """
      INSERT OR REPLACE INTO guest_stays 
      (id, apartment, from_date, to_date, last_4_digits, link, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?)
      """

  _ <- SQLite3.queryDB conn sql
    [ unsafeToForeign id
    , unsafeToForeign apartmentStr
    , unsafeToForeign fromDateStr
    , unsafeToForeign toDateStr
    , unsafeToForeign last4Digits
    , unsafeToForeign link
    , unsafeToForeign "" -- notes - empty for now
    ]

  pure guestStay

sqliteDeleteGuestStay :: SQLite3.DBConnection -> String -> Aff Boolean
sqliteDeleteGuestStay conn guestStayId = do
  liftEffect $ Console.log $ "Deleting guest stay: " <> guestStayId

  let
    sql = "DELETE FROM guest_stays WHERE id = ?"

  _ <- SQLite3.queryDB conn sql [ unsafeToForeign guestStayId ]
  pure true

sqliteFetchStoredGuestStays :: SQLite3.DBConnection -> Apartment -> Aff (Array GuestStay)
sqliteFetchStoredGuestStays conn apartment = do
  let
    apartmentStr = case apartment of
      Apartment name -> name

    sql =
      """
      SELECT id, apartment, from_date, to_date, last_4_digits, link, notes
      FROM guest_stays
      WHERE apartment = ?
      ORDER BY from_date ASC
      """

  results <- (decodeJson <<< unsafeFromForeign) <$> SQLite3.queryDB conn sql
    [ unsafeToForeign apartmentStr ]

  case results of
    Right (rawGuestStays :: Array RawGuestStay) -> do
      guestStays <- traverse convertRawGuestStay rawGuestStays
      pure guestStays

    Left e ->
      throwError (error $ "Failed to fetch stored guest stays: " <> show e)
  where
  convertRawGuestStay :: RawGuestStay -> Aff GuestStay
  convertRawGuestStay (RawGuestStay { id, apartment: apartmentStr, from_date, to_date, last_4_digits, link }) = do
    parsedFromDate <- parseDate from_date
    parsedToDate <- parseDate to_date

    pure
      { id: id
      , apartment: Apartment apartmentStr
      , fromDate: parsedFromDate
      , toDate: parsedToDate
      , last4Digits: last_4_digits
      , link: link
      , source: Internal
      }

  parseDate :: String -> Aff Date.Date
  parseDate dateStr = do
    case traverse Int.fromString (String.split (Pattern "-") dateStr) of
      Just [ yearStr, monthStr, dayStr ] ->
        Maybe.fromMaybe (throwError $ error $ "Invalid date format: " <> dateStr) do
          year <- toEnum yearStr
          month <- toEnum monthStr
          day <- toEnum dayStr

          pure (pure $ Date.canonicalDate year month day)

      _ -> throwError (error $ "Invalid date format: " <> dateStr)

sqliteFetchGuestStayById :: SQLite3.DBConnection -> String -> Aff (Maybe GuestStay)
sqliteFetchGuestStayById conn guestStayId = do
  liftEffect $ Console.log $ "Fetching guest stay by ID: " <> guestStayId

  let
    sql =
      """
      SELECT id, apartment, from_date, to_date, last_4_digits, link, notes
      FROM guest_stays
      WHERE id = ?
      LIMIT 1
      """

  results <- (decodeJson <<< unsafeFromForeign) <$> SQLite3.queryDB conn sql
    [ unsafeToForeign guestStayId ]

  case results of
    Right (rawGuestStays :: Array RawGuestStay) ->
      case rawGuestStays of
        [ rawGuestStay ] -> do
          guestStay <- convertRawGuestStay rawGuestStay
          pure $ Just guestStay
        [] -> pure Nothing
        _ -> throwError (error "Multiple guest stays found with same ID")

    Left e ->
      throwError (error $ "Failed to fetch guest stay by ID: " <> show e)
  where
  convertRawGuestStay :: RawGuestStay -> Aff GuestStay
  convertRawGuestStay (RawGuestStay { id, apartment: apartmentStr, from_date, to_date, last_4_digits, link }) = do
    parsedFromDate <- parseDate from_date
    parsedToDate <- parseDate to_date

    pure
      { id: id
      , apartment: Apartment apartmentStr
      , fromDate: parsedFromDate
      , toDate: parsedToDate
      , last4Digits: last_4_digits
      , link: link
      , source: Internal
      }

  parseDate :: String -> Aff Date.Date
  parseDate dateStr = do
    case traverse Int.fromString (String.split (Pattern "-") dateStr) of
      Just [ yearStr, monthStr, dayStr ] ->
        Maybe.fromMaybe (throwError $ error $ "Invalid date format: " <> dateStr) do
          year <- toEnum yearStr
          month <- toEnum monthStr
          day <- toEnum dayStr

          pure (pure $ Date.canonicalDate year month day)

      _ -> throwError (error $ "Invalid date format: " <> dateStr)