module Airbnbeast.Cleaning (CleaningWeekend(..), CleaningWindow(..), TimeOfDay(..), TimeBlock(..), prettyPrintWindow, scheduleFromGuestStays, scheduleFromGuestStaysWithDate, cleaningWindowToTimeBlocks) where

import Prelude

import Airbnbeast.Availability (Apartment, GuestStay)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Date (Date, Weekday(..))
import Data.Date as Date
import Data.DateTime (DateTime(..), Time(..))
import Data.DateTime as DateTime
import Data.Enum (enumFromTo, fromEnum, toEnum)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Days(..))
import Data.Tuple.Nested ((/\))

data CleaningWeekend
  = AllWeekend
  | PartialWeekend
  | NoWeekend

derive instance Generic CleaningWeekend _
derive instance Eq CleaningWeekend

instance Show CleaningWeekend where
  show = genericShow

newtype CleaningWindow = CleaningWindow
  { from :: DateTime
  , to :: DateTime
  , weekend :: CleaningWeekend
  , stay :: GuestStay
  , timeBlocks :: NonEmptyArray TimeBlock
  }

derive instance Newtype CleaningWindow _
derive instance Generic CleaningWindow _

instance Show CleaningWindow where
  show = genericShow

data TimeOfDay
  = Morning
  | Afternoon

derive instance Generic TimeOfDay _
derive instance Eq TimeOfDay
derive instance Ord TimeOfDay

instance Show TimeOfDay where
  show = genericShow

newtype TimeBlock = TimeBlock
  { date :: Date
  , timeOfDay :: TimeOfDay
  , available :: Boolean
  , apartment :: Apartment
  }

derive instance Newtype TimeBlock _
derive instance Generic TimeBlock _
derive instance Eq TimeBlock

instance Show TimeBlock where
  show = genericShow

newtype All = All Boolean
newtype Any = Any Boolean

derive instance Newtype All _
derive instance Newtype Any _

instance Semigroup All where
  append (All a) (All b) = All (a && b)

instance Monoid All where
  mempty = All true

instance Semigroup Any where
  append (Any a) (Any b) = Any (a || b)

instance Monoid Any where
  mempty = Any false

prettyPrintWindow :: CleaningWindow -> String
prettyPrintWindow (CleaningWindow { from, to, weekend, stay }) =
  prettyDate from <> " → " <> prettyDate to <> "\n"
    <> "-----------------------\n"
    <> "Code: "
    <> stay.last4Digits
    <> "\n"
    <> "Link: "
    <> stay.link
    <> "\n"
    <> "Weekend: "
    <> show weekend

  where
  prettyDate :: DateTime -> String
  prettyDate dt = do
    let date = DateTime.date dt

    (show $ fromEnum $ DateTime.year date) <> "-" <> (show $ fromEnum $ DateTime.month date) <> "-" <> (show $ fromEnum $ DateTime.day date) <> " (" <> (show $ DateTime.weekday date) <> ")"

scheduleFromGuestStays :: Map Apartment (Array GuestStay) -> Map Apartment (Array CleaningWindow)
scheduleFromGuestStays =
  (Map.toUnfoldable :: _ -> Array _)
    >>> map (\(apartment /\ stays) -> apartment /\ windowsBetweenStays [] stays)
    >>>
      Map.fromFoldable
  where
  windowsBetweenStays :: Array CleaningWindow -> Array GuestStay -> Array CleaningWindow
  windowsBetweenStays windows stays = case (windows /\ Array.uncons stays) of
    ([] /\ Just { head, tail }) -> Maybe.fromMaybe [] do
      first <- firstWindow head

      pure $ windowsBetweenStays [ first ] tail

    (ws /\ Just { head, tail }) -> Maybe.fromMaybe [] do
      previousWindow <- Array.last ws
      window <- windowBetween head (unwrap previousWindow).stay.toDate head.fromDate

      pure $ windowsBetweenStays (Array.snoc ws window) tail

    (ws /\ Nothing) ->
      ws

  firstWindow :: GuestStay -> Maybe CleaningWindow
  firstWindow stay@{ fromDate } = do
    newGuestStart <- Date.adjust (Days (-1.0)) fromDate

    windowBetween stay newGuestStart fromDate

  windowBetween :: GuestStay -> Date -> Date -> Maybe CleaningWindow
  windowBetween stay startDate endDate = do
    cleaningStart <- atCleaningStart startDate
    cleaningEnd <- atCleaningEnd endDate

    let
      weekend = weekendCoverage startDate endDate
      timeBlocks = cleaningWindowToTimeBlocks cleaningStart cleaningEnd stay

    case NEArray.fromArray timeBlocks of
      Just nonEmptyTimeBlocks -> pure $ CleaningWindow
        { from: cleaningStart
        , to: cleaningEnd
        , weekend
        , stay
        , timeBlocks: nonEmptyTimeBlocks
        }
      Nothing -> Nothing -- This shouldn't happen if the window is valid

  atCleaningStart :: Date -> Maybe DateTime
  atCleaningStart refDate = do
    time <- Time <$> toEnum 13 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0

    pure $ DateTime refDate time

  atCleaningEnd :: Date -> Maybe DateTime
  atCleaningEnd refDate = do
    time <- Time <$> toEnum 13 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0

    pure $ DateTime refDate time

  isWeekend :: Date -> Boolean
  isWeekend d = case Date.weekday d of
    Saturday -> true
    Sunday -> true
    _ -> false

  weekendCoverage startDate endDate =
    let
      dateSpan = enumFromTo startDate endDate :: Array Date
    in
      if unwrap (foldMap (All <<< isWeekend) dateSpan) then AllWeekend
      else if unwrap (foldMap (Any <<< isWeekend) dateSpan) then PartialWeekend
      else NoWeekend

scheduleFromGuestStaysWithDate :: Date -> Map Apartment (Array GuestStay) -> Map Apartment (Array CleaningWindow)
scheduleFromGuestStaysWithDate currentDate guestStays =
  let
    allWindows = scheduleFromGuestStays guestStays
  in
    map (Array.filter (isValidCleaningWindow currentDate)) allWindows

isValidCleaningWindow :: Date -> CleaningWindow -> Boolean
isValidCleaningWindow currentDate (CleaningWindow { to }) =
  let
    cleaningEndDate = DateTime.date to
  in
    -- Only include windows that haven't completely passed
    -- (cleaning window end date must be today or in the future)
    cleaningEndDate >= currentDate

-- Convert cleaning window parameters to a series of TimeBlocks
cleaningWindowToTimeBlocks :: DateTime -> DateTime -> GuestStay -> Array TimeBlock
cleaningWindowToTimeBlocks from to stay =
  let
    startDate = DateTime.date from
    endDate = DateTime.date to
    allDates = enumFromTo startDate endDate
    apartment = stay.apartment
  in
    Array.concatMap (dateToTimeBlocks apartment) allDates
  where
  dateToTimeBlocks :: Apartment -> Date -> Array TimeBlock
  dateToTimeBlocks apt date =
    let
      startDate = DateTime.date from
      endDate = DateTime.date to

      morningBlock = TimeBlock
        { date
        , timeOfDay: Morning
        , available: true -- Default to available, manual overrides will set to false
        , apartment: apt
        }

      afternoonBlock = TimeBlock
        { date
        , timeOfDay: Afternoon
        , available: true
        , apartment: apt
        }
    in
      if date == startDate && date == endDate then
        -- Single day window: include both morning and afternoon if within time range
        [ morningBlock, afternoonBlock ]
      else if date == startDate then
        -- First day: start from afternoon (13:00)
        [ afternoonBlock ]
      else if date == endDate then
        -- Last day: only morning (until 13:00)
        [ morningBlock ]
      else
        -- Middle days: both morning and afternoon
        [ morningBlock, afternoonBlock ]

