module Airbnbeast.Storage
  ( Storage
  , sqliteEnableTimeBlock
  , sqliteStorage
  ) where

import Prelude

import Airbnbeast.Availability (Apartment(..))
import Airbnbeast.Cleaning (CleaningWindow(..), TimeBlock(..), TimeOfDay(..))
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Date as Date
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign (unsafeFromForeign, unsafeToForeign)
import SQLite3 as SQLite3

type Storage =
  { disableTimeBlock :: TimeBlock -> Aff TimeBlock
  , enableTimeBlock :: TimeBlock -> Aff TimeBlock
  , disabledTimeBlocksDuringStay :: CleaningWindow -> Aff (Array TimeBlock)
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

sqliteStorage :: SQLite3.DBConnection -> Storage
sqliteStorage conn =
  { disableTimeBlock: sqliteDisableTimeBlock conn
  , enableTimeBlock: sqliteEnableTimeBlock conn
  , disabledTimeBlocksDuringStay: sqliteDisabledTimeBlocksDuringStay conn
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
    case String.split (Pattern "-") dateStr of
      [ yearStr, monthStr, dayStr ] -> do
        case { year: Int.fromString yearStr, month: Int.fromString monthStr, day: Int.fromString dayStr } of
          { year: Just y, month: Just m, day: Just d } -> do
            case { year: toEnum y, month: toEnum m, day: toEnum d } of
              { year: Just year, month: Just month, day: Just day } -> do
                pure $ Date.canonicalDate year month day
              _ -> throwError (error $ "Invalid date components: " <> dateStr)
          _ -> throwError (error $ "Invalid date format: " <> dateStr)
      _ -> throwError (error $ "Invalid date format: " <> dateStr)

  parseTimeOfDay :: String -> Aff TimeOfDay
  parseTimeOfDay "Morning" = pure Morning
  parseTimeOfDay "Afternoon" = pure Afternoon
  parseTimeOfDay other = throwError (error $ "Invalid time of day: " <> other)

