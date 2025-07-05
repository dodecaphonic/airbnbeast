module Airbnbeast.Storage
  ( Storage
  , sqliteEnableTimeBlock
  , sqliteStorage
  ) where

import Prelude

import Airbnbeast.Availability (Apartment(..), GuestStay)
import Airbnbeast.Cleaning (TimeBlock(..), TimeOfDay)
import Data.Date as Date
import Data.Enum (fromEnum)
import Data.Newtype (unwrap)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign (unsafeToForeign)
import SQLite3 as SQLite3

type Storage =
  { disableTimeBlock :: TimeBlock -> Aff TimeBlock
  , enableTimeBlock :: TimeBlock -> Aff TimeBlock
  , disabledTimeBlocksDuringStay :: GuestStay -> Aff (Array TimeBlock)
  }

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
    
    sql = """
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
    
    sql = """
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

sqliteDisabledTimeBlocksDuringStay :: SQLite3.DBConnection -> GuestStay -> Aff (Array TimeBlock)
sqliteDisabledTimeBlocksDuringStay _ _ = throwError (error "Oh no")

