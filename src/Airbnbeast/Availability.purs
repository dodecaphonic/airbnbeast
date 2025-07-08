module Airbnbeast.Availability (GuestStay, Apartment(..), fetchAvailability, fetchDayToDay, fetchGuestStays) where

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
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Node.ICal as ICal

newtype Apartment = Apartment String

derive newtype instance Eq Apartment
derive newtype instance Ord Apartment
derive newtype instance Show Apartment

type AirbnbCalendar =
  { apartment :: Apartment
  , icsUrl :: String
  }

type GuestStay =
  { last4Digits :: String
  , apartment :: Apartment
  , fromDate :: Date
  , toDate :: Date
  , link :: String
  , id :: String
  }

type DailyOccupancy =
  { date :: Date
  , occupancy :: Array String
  }

type AirbnbDatesWithApartment =
  { dates :: AirbnbDates
  , apartment :: Apartment
  }

fetchAvailability :: AirbnbCalendar -> Aff (Array AirbnbDatesWithApartment)
fetchAvailability { apartment, icsUrl } = do
  calendar <- ICal.fetchICS icsUrl

  case calendar of
    Left (NonEmptyList errs) ->
      throwError (error $ NEL.intercalate "\n" $ unwrap <$> errs)

    Right { events } ->
      case traverse Parser.parseEvent events of
        Left errs ->
          throwError (error errs)

        Right events' ->
          pure $ (\dates -> { dates, apartment }) <$> events'

airbnbDatesToGuestStays :: Array AirbnbDatesWithApartment -> Map Apartment (Array GuestStay)
airbnbDatesToGuestStays = foldl dateAsGuestStay Map.empty
  where
  dateAsGuestStay gss { dates, apartment } = case dates of
    Parser.Reservation r ->
      let
        guestStay =
          { apartment
          , fromDate: r.from
          , toDate: r.to
          , last4Digits: r.last4Digits
          , link: r.link
          , id: idFromLink r.link
          }
      in
        Map.alter
          ( case _ of
              Just ads -> Just $ Array.snoc ads guestStay
              Nothing -> Just [ guestStay ]
          )
          apartment
          gss
    Parser.Unavailability _ -> gss

  idFromLink link = Maybe.fromMaybe "-" $ Array.last (String.split (Pattern "/") link)

airbnbDatesToDailyOccupancy :: Array AirbnbDatesWithApartment -> Array DailyOccupancy
airbnbDatesToDailyOccupancy dates = datesAsDailyOccupancy
  where
  dateMap :: Map.Map Date (Array AirbnbDatesWithApartment)
  dateMap = foldl addDates Map.empty dates

  addDates map dwan =
    foldr (Map.alter (mergeDates dwan)) map $
      (enumFromTo (dwan.dates ^. fromL) (dwan.dates ^. toL) :: Array Date)

  mergeDates dwan Nothing = Just [ dwan ]
  mergeDates dwan (Just existingDates) =
    case Array.find (\{ apartment } -> apartment == dwan.apartment) existingDates of
      Just odwan -> case Tuple dwan.dates odwan.dates of
        Parser.Reservation _ /\ Parser.Unavailability _ ->
          Just $ (flip Array.snoc) dwan $ Array.delete odwan existingDates

        _ ->
          Just existingDates

      Nothing ->
        Just $ Array.snoc existingDates dwan

  asDailyOccuppancy (date /\ airbnbDates) =
    let
      occupancy = airbnbDates
        <#> \({ dates, apartment: (Apartment apartmentName) }) ->
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

fetchGuestStays :: Array AirbnbCalendar -> Aff (Map Apartment (Array GuestStay))
fetchGuestStays = traverse fetchAvailability >>> map (foldMap identity) >>> map airbnbDatesToGuestStays
