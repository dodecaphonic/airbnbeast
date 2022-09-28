module Node.ICal (Calendar, DateWithTimezone, Event, Settings, ParseICSError(..), Status(..), parseICS, fetchICS) where

import Prelude

import Control.Monad.Except (ExceptT, runExcept, runExceptT, throwError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Identity (Identity)
import Data.JSDate as JSDate
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Foreign (Foreign)
import Foreign as F
import Foreign.Index ((!))
import Foreign.Object (Object)
import Foreign.Object as FO

type RawCalendarObject = Object Foreign

type Settings =
  { version :: String
  , scale :: String
  }

type DateWithTimezone = { date :: DateTime, tz :: Maybe String }

data Status
  = Tentative
  | Confirmed
  | Cancelled
  | NeedsAction
  | Completed
  | InProcess
  | Draft
  | Final

derive instance Eq Status

type Event =
  { summary :: String
  , location :: Maybe String
  , description :: Maybe String
  , startAt :: DateWithTimezone
  , endAt :: DateWithTimezone
  , uid :: String
  , status :: Maybe Status
  }

type Calendar =
  { settings :: Settings
  , events :: Array Event
  }

parseICS :: String -> ICSParser Calendar
parseICS = runFn1 _parseICS >>> calendarFromObject

fetchICS :: String -> Aff (ICSParser Calendar)
fetchICS url = do
  cal <- Promise.toAff $ runFn1 _fetchICS url

  pure $ calendarFromObject cal

foreign import _parseICS :: Fn1 String RawCalendarObject
foreign import _fetchICS :: Fn1 String (Promise RawCalendarObject)

newtype ParseICSError = ParseICSError String

derive instance Newtype ParseICSError _

instance Show ParseICSError where
  show (ParseICSError e) = e

instance Show Status where
  show Tentative = "TENTATIVE"
  show Confirmed = "CONFIRMED"
  show Cancelled = "CANCELLED"
  show NeedsAction = "NEEDS-ACTION"
  show Completed = "COMPLETED"
  show InProcess = "IN-PROCESS"
  show Draft = "DRAFT"
  show Final = "FINAL"

type ICSParser = Either (NonEmptyList ParseICSError)

type ForeignStack = ExceptT (NEL.NonEmptyList F.ForeignError) Identity

calendarFromObject :: RawCalendarObject -> ICSParser Calendar
calendarFromObject obj = withMappedErrors do
  settings <- parseSettings
  events <- parseEvents

  pure { settings, events }

  where
  withMappedErrors :: forall a. ForeignStack a -> ICSParser a
  withMappedErrors parsed = case unwrap $ runExceptT parsed of
    Left errors -> Left (ParseICSError <<< mapError <$> errors)

    Right value -> pure value

    where
    mapError (F.ForeignError e) = e
    mapError (F.TypeMismatch f e) = "Type mismatch at " <> f <> " | " <> e
    mapError (F.ErrorAtIndex i e) = "Error at index " <> show i <> ": " <> show e
    mapError (F.ErrorAtProperty p e) = "Error at property '" <> p <> "': " <> show e

  parseSettings =
    case FO.lookup "vcalendar" obj of
      Nothing ->
        throwError $ NEL.singleton (F.ForeignError "Missing key 'vcalendar'")

      Just vcal -> do
        version <- vcal ! "version" >>= F.readString
        scale <- vcal ! "calscale" >>= F.readString

        pure { version, scale }

  nonEventKeys = Set.fromFoldable [ "vcalendar", "prodid" ]

  parseEvents =
    (FO.toUnfoldable obj :: Array (Tuple String Foreign))
      # foldMap (\ev -> if Set.member (Tuple.fst ev) nonEventKeys then mempty else pure (Tuple.snd ev))
      # traverse parseEvent

  parseEvent ev = do
    summary <- ev ! "summary" >>= F.readString
    location <- ev ! "location" >>= F.readNullOrUndefined >>= traverse F.readString
    description <- ev ! "description" >>= F.readNullOrUndefined >>= traverse F.readString
    uid <- ev ! "uid" >>= F.readString
    startAt <- ev ! "start" >>= readDateWithTimezone
    endAt <- ev ! "end" >>= readDateWithTimezone
    status <- ev ! "status" >>= F.readNullOrUndefined >>= traverse readStatus

    pure { summary, location, description, uid, startAt, endAt, status }

  readDateWithTimezone dt = do
    date <- JSDate.toDateTime <$> readDate
    tz <- dt ! "tz" >>= F.readNullOrUndefined >>= traverse F.readString

    case date of
      Just date' ->
        pure { date: date', tz }

      Nothing ->
        throwError $ NEL.singleton (F.ForeignError "Could not parse date")

    where
    readDate :: ForeignStack JSDate.JSDate
    readDate = case (runExcept $ JSDate.readDate dt) of
      Left errs -> throwError errs
      Right date' -> pure date'

  readStatus :: Foreign -> ForeignStack Status
  readStatus s = F.readString s >>= \status -> case status of
    "CANCELLED" -> pure Cancelled
    "TENTATIVE" -> pure Tentative
    "CONFIRMED" -> pure Confirmed
    "NEEDS-ACTION" -> pure NeedsAction
    "COMPLETED" -> pure Completed
    "IN-PROCESS" -> pure InProcess
    "DRAFT" -> pure Draft
    "FINAL" -> pure Final
    _ -> throwError $ NEL.singleton (F.ForeignError ("Could not parse status " <> status))
