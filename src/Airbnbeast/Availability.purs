module Airbnbeast.Availability (fetchAvailability, fetchDayToDay) where

import Prelude

import Airbnbeast.Parser (AirbnbDates, fromL, toL)
import Airbnbeast.Parser as Parser
import Data.Array (foldMap)
import Data.Array as Array
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Foldable (foldl, foldr)
import Data.Lens ((^.))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Node.ICal as ICal

type AirbnbCalendar =
  { name :: String
  , icsUrl :: String
  }

type DailyOccupancy =
  { date :: Date
  , occupancy :: Array String
  }

type AirbnbDatesWithApartmentName =
  { dates :: AirbnbDates
  , apartmentName :: String
  }

fetchAvailability :: AirbnbCalendar -> Aff (Array AirbnbDatesWithApartmentName)
fetchAvailability { name: apartmentName, icsUrl } = do
  calendar <- ICal.fetchICS icsUrl

  case calendar of
    Left (NonEmptyList errs) ->
      throwError (error $ NEL.intercalate "\n" $ unwrap <$> errs)

    Right { events } ->
      case traverse Parser.parseEvent events of
        Left errs ->
          throwError (error errs)

        Right events' ->
          pure $ (\dates -> { dates, apartmentName }) <$> events'

airbnbDatesToDailyOccupancy :: Array AirbnbDatesWithApartmentName -> Array DailyOccupancy
airbnbDatesToDailyOccupancy dates = datesAsDailyOccupancy
  where
  dateMap :: Map.Map Date (Array AirbnbDatesWithApartmentName)
  dateMap = foldl addDates Map.empty dates

  addDates map dwan =
    foldr (Map.alter (mergeDates dwan)) map $
      (enumFromTo (dwan.dates ^. fromL) (dwan.dates ^. toL) :: Array Date)

  mergeDates dwan Nothing = Just [ dwan ]
  mergeDates dwan (Just existingDates) = Just $ Array.snoc existingDates dwan

  asDailyOccuppancy (date /\ airbnbDates) =
    let
      occupancy = airbnbDates
        <#> \({ dates, apartmentName }) ->
          case dates of
            Parser.Reservation { last4Digits } ->
              apartmentName <> ": Reserved (" <> last4Digits <> ")"

            Parser.Unavailability _ ->
              apartmentName <> ": Unavailable"
    in
      { date, occupancy }

  datesAsDailyOccupancy =
    asDailyOccuppancy <$> Map.toUnfoldable dateMap

fetchDayToDay :: Array AirbnbCalendar -> Aff (Array DailyOccupancy)
fetchDayToDay = traverse fetchAvailability >>> map (foldMap identity) >>> map airbnbDatesToDailyOccupancy
