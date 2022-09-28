module Airbnbeast.Parser (AirbnbDates(..), fromL, toL, parseEvent) where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Date (Date)
import Data.DateTime as DT
import Data.Either (Either(..))
import Data.Either as Either
import Data.Lens (Lens', lens)
import Data.Maybe as Maybe
import Data.String.Regex as Regex
import Node.ICal (Event)

data AirbnbDates
  = Reservation
      { from :: Date
      , to :: Date
      , link :: String
      , last4Digits :: String
      }
  | Unavailability { from :: Date, to :: Date }

instance Show AirbnbDates where
  show (Reservation r) = "Reservation " <> show r
  show (Unavailability r) = "Unavailability " <> show r

derive instance Eq AirbnbDates

fromL :: Lens' AirbnbDates Date
fromL = lens get set
  where
    get (Reservation { from }) = from
    get (Unavailability { from }) = from
    set (Reservation r) from = Reservation (r { from = from })
    set (Unavailability r) from = Unavailability (r { from = from })

toL :: Lens' AirbnbDates Date
toL = lens get set
  where
    get (Reservation { to }) = to
    get (Unavailability { to }) = to
    set (Reservation r) to = Reservation (r { to = to })
    set (Unavailability r) to = Unavailability (r { to = to })

parseEvent :: Event -> Either String AirbnbDates
parseEvent event = do
  let
    from = DT.date event.startAt.date
    to = DT.date event.endAt.date

  case event.summary of
    "Airbnb (Not available)" ->
      pure $ Unavailability { from, to }

    "Reserved" -> do
      description <- Maybe.maybe (Left "Missing description") Right event.description

      let
        link = match "Reservation URL: (\\S+)" "Could not match link" description
        last4Digits = match "\\(Last 4 Digits\\): (\\d{4})" "Could not match last 4 digits" description

      reservation <$> pure from <*> pure to <*> last4Digits <*> link

    other -> do
      Left $ "Unknown event type: " <> other

  where
  reservation from to last4Digits link =
    Reservation { from, to, last4Digits, link }

  match rxStr errorMsg str = do
    rx <- Regex.regex rxStr mempty

    Either.note errorMsg (NEA.last =<< Regex.match rx str)
