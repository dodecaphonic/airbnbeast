module Airbnbeast.WebServer where

import Prelude

import Airbnbeast.Availability (Apartment(..), fetchGuestStays)
import Airbnbeast.Cleaning as Cleaning
import Airbnbeast.Html as Html
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPure (Request, ResponseM, ServerM, header, notFound, ok', serve)
import HTTPure.Method (Method(..))

type Routes = Request -> ResponseM

normalizeApartmentName :: String -> Maybe Apartment
normalizeApartmentName "gloria" = Just (Apartment "Glória")
normalizeApartmentName "santa" = Just (Apartment "Santa")
normalizeApartmentName _ = Nothing

routes :: Routes
routes { method: Get, path: [] } = do
  liftEffect $ log "Serving index page"
  ok' (header "Content-Type" "text/html") Html.indexPage

routes { method: Get, path: [ "schedule" ] } = do
  liftEffect $ log "Serving full cleaning schedule"
  schedule <- fetchCleaningSchedule
  ok' (header "Content-Type" "text/html") (Html.cleaningSchedulePage schedule)

routes { method: Get, path: [ "apartment", apartmentName ] } = do
  liftEffect $ log $ "Serving apartment page for: " <> apartmentName
  schedule <- fetchCleaningSchedule
  case normalizeApartmentName apartmentName of
    Just apartment ->
      case Map.lookup apartment schedule of
        Just windows ->
          ok' (header "Content-Type" "text/html") (Html.apartmentPage apartment windows)
        Nothing ->
          notFound
    Nothing ->
      notFound

routes _ = do
  liftEffect $ log "404 - Page not found"
  notFound

fetchCleaningSchedule :: Aff (Map.Map Apartment (Array Cleaning.CleaningWindow))
fetchCleaningSchedule = do
  let
    gloria =
      { apartment: Apartment "Glória"
      , icsUrl: "https://www.airbnb.com/calendar/ical/47420131.ics?s=317a8c2653c6a64a4cc4ed1ad89f6afd"
      }
    santa =
      { apartment: Apartment "Santa"
      , icsUrl: "https://www.airbnb.com/calendar/ical/568955469596249266.ics?s=878a19388f719853be173bf4ad3dd77c"
      }

  guestStays <- fetchGuestStays [ gloria, santa ]
  pure $ Cleaning.scheduleFromGuestStays guestStays

startServer :: Int -> ServerM
startServer port = do
  log $ "🚀 Airbnbeast web server starting on port " <> show port
  log "📋 Available routes:"
  log "  / - Home page"
  log "  /schedule - Full cleaning schedule"
  log "  /apartment/:name - Apartment-specific schedule"
  log ""
  serve port routes do
    log $ "✅ Server is running on http://localhost:" <> show port