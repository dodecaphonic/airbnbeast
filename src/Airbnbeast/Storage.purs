module Airbnbeast.Storage
  ( Storage
  , sqliteEnableTimeBlock
  , sqliteStorage
  ) where

import Airbnbeast.Availability (GuestStay)
import Airbnbeast.Cleaning (TimeBlock)
import Effect.Aff (Aff, error, throwError)
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
sqliteDisableTimeBlock _ _ = throwError (error "Oh no")

sqliteEnableTimeBlock :: SQLite3.DBConnection -> TimeBlock -> Aff TimeBlock
sqliteEnableTimeBlock _ _ = throwError (error "Oh no")

sqliteDisabledTimeBlocksDuringStay :: SQLite3.DBConnection -> GuestStay -> Aff (Array TimeBlock)
sqliteDisabledTimeBlocksDuringStay _ _ = throwError (error "Oh no")

