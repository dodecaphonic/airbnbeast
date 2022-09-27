module Test.Main where

import Prelude

import Airbnbeast.Parser (AirbnbDates(..))
import Airbnbeast.Parser as Airbnbeast
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.DateTime as DT
import Data.Either (Either(..))
import Data.Either as Either
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Aff (Error, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.ICal (Status(..))
import Node.ICal as ICal
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  nodeICalSpecs
  airbnbeastSpecs

nodeICalSpecs :: forall m g. Monad m => MonadThrow Error g => MonadEffect g => SpecT g Unit m Unit
nodeICalSpecs = do
  describe "Node.ICal" do
    describe "parseICS" do
      it "parses a sample string correctly" do
        let ics = """
BEGIN:VCALENDAR
VERSION:2.0
CALSCALE:GREGORIAN
BEGIN:VEVENT
SUMMARY:Hey look! An example event!
DTSTART;TZID=America/New_York:20130802T103400
DTEND;TZID=America/New_York:20130802T110400
LOCATION:1000 Broadway Ave.\, Brooklyn
DESCRIPTION: Do something in NY.
STATUS:CONFIRMED
UID:7014-1567468800-1567555199@peterbraden@peterbraden.co.uk
END:VEVENT
END:VCALENDAR
"""

        case ICal.parseICS ics of
          Left errs ->
            fail $ "Expected it to not fail, but it did: " <> show errs

          Right { events, settings } -> do
            settings `shouldEqual` { version: "2.0", scale: "GREGORIAN" }

            let startAt = DT.DateTime
                       <$> (DT.canonicalDate <$> toEnum 2013 <*> toEnum 8 <*> toEnum 2)
                       <*> (DT.Time <$> toEnum 14 <*> toEnum 34 <*> toEnum 0 <*> toEnum 0)
                endAt = DT.DateTime
                     <$> (DT.canonicalDate <$> toEnum 2013 <*> toEnum 8 <*> toEnum 2)
                     <*> (DT.Time <$> toEnum 15 <*> toEnum 4 <*> toEnum 0 <*> toEnum 0)
                desiredOutput = (\s e ->
                                  [{ summary: "Hey look! An example event!"
                                  , startAt: { date: s, tz: Just "America/New_York" }
                                  , endAt: { date: e, tz: Just "America/New_York" }
                                  , description: Just " Do something in NY."
                                  , uid: "7014-1567468800-1567555199@peterbraden@peterbraden.co.uk"
                                  , status: Just Confirmed
                                  , location: Just "1000 Broadway Ave., Brooklyn"
                                 }]
                                ) <$> startAt <*> endAt

            events `shouldEqual` (Maybe.fromMaybe [] desiredOutput)

    it "parses a more involved definition" do
      ics <- liftEffect $ Buffer.toString Encoding.UTF8 =<< FS.readFile "test/fixtures/airbnb.ics"

      case ICal.parseICS ics of
        Left errs ->
          fail $ "Expected it to not fail, but it did: " <> show errs

        Right { events } ->
          Array.null events `shouldEqual` false

airbnbeastSpecs :: forall m g. Monad m => MonadThrow Error g => SpecT g Unit m Unit
airbnbeastSpecs = do
  describe "Airbnbeast.Parser" do
    describe "parseEvent" do
      it "turns a (Not available) event into an Unavailability" do
        let
          startAt = DT.DateTime
                      <$> (DT.canonicalDate <$> toEnum 2013 <*> toEnum 8 <*> toEnum 2)
                      <*> (DT.Time <$> toEnum 14 <*> toEnum 34 <*> toEnum 0 <*> toEnum 0)

          endAt = DT.DateTime
                 <$> (DT.canonicalDate <$> toEnum 2013 <*> toEnum 8 <*> toEnum 2)
                 <*> (DT.Time <$> toEnum 15 <*> toEnum 4 <*> toEnum 0 <*> toEnum 0)

          event = (\s e ->
                    { startAt: { date: s, tz: Nothing }
                    , endAt: { date: e, tz: Nothing }
                    , summary: "Airbnb (Not available)"
                    , description: Nothing
                    , location: Nothing
                    , status: Nothing
                    , uid: "uid@eventy@example.com"
                    } :: ICal.Event
                  )
                  <$> startAt
                  <*> endAt
                  # Either.note "Failed to create sample event"

          parsedEvent = (\s e -> Unavailability { from: DT.date s, to: DT.date e }) <$> startAt <*> endAt
                        # Either.note "Failed to create sample Unavailability"

        (event >>= Airbnbeast.parseEvent) `shouldEqual` parsedEvent

      it "turns a Reserved event into a Reservation" do
        let
          startAt = DT.DateTime
                      <$> (DT.canonicalDate <$> toEnum 2013 <*> toEnum 8 <*> toEnum 2)
                      <*> (DT.Time <$> toEnum 14 <*> toEnum 34 <*> toEnum 0 <*> toEnum 0)

          endAt = DT.DateTime
                 <$> (DT.canonicalDate <$> toEnum 2013 <*> toEnum 8 <*> toEnum 2)
                 <*> (DT.Time <$> toEnum 15 <*> toEnum 4 <*> toEnum 0 <*> toEnum 0)

          event = (\s e ->
                    { startAt: { date: s, tz: Nothing }
                    , endAt: { date: e, tz: Nothing }
                    , summary: "Reserved"
                    , description: Just "Reservation URL: https://www.airbnb.com/hosting/reservations/details/HMMTAEZPCA\nPhone Number (Last 4 Digits): 5757"
                    , location: Nothing
                    , status: Nothing
                    , uid: "uid@eventy@example.com"
                    } :: ICal.Event
                  )
                  <$> startAt
                  <*> endAt
                  # Either.note "Failed to create sample event"

          parsedEvent = (\s e -> Airbnbeast.Reservation { from: DT.date s, to: DT.date e, last4Digits: "5757", link: "https://www.airbnb.com/hosting/reservations/details/HMMTAEZPCA" }) <$> startAt <*> endAt
                        # Either.note "Failed to create sample Unavailability"

        (event >>= Airbnbeast.parseEvent) `shouldEqual` parsedEvent
