module Airbnbeast.Storage
  ( Storage
  , sqliteEnableTimeBlock
  , sqliteStorage
  ) where

import Prelude

import Airbnbeast.Availability (Apartment(..))
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

sqliteStorage :: SQLite3.DBConnection -> Storage
sqliteStorage conn =
  { disableTimeBlock: sqliteDisableTimeBlock conn
  , enableTimeBlock: sqliteEnableTimeBlock conn
  , disabledTimeBlocksDuringStay: sqliteDisabledTimeBlocksDuringStay conn
  , authenticateUser: sqliteAuthenticateUser conn
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

