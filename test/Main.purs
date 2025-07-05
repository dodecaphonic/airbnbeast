module Test.Main where

import Prelude

import Airbnbeast.Availability (Apartment(..))
import Airbnbeast.Cleaning (CleaningWeekend(..), CleaningWindow(..))
import Airbnbeast.Cleaning as Cleaning
import Airbnbeast.Parser (AirbnbDates(..))
import Airbnbeast.Parser as Airbnbeast
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array as Array
import Data.DateTime as DT
import Data.Either (Either(..))
import Data.Either as Either
import Data.Enum (toEnum)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
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
main = launchAff_ $ runSpec [ consoleReporter ] do
  nodeICalSpecs
  airbnbeastSpecs
  cleaningSpecs

nodeICalSpecs :: forall m g. Monad m => MonadThrow Error g => MonadEffect g => SpecT g Unit m Unit
nodeICalSpecs = do
  describe "Node.ICal" do
    describe "parseICS" do
      it "parses a sample string correctly" do
        let
          ics =
            """
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

            let
              startAt = DT.DateTime
                <$> (DT.canonicalDate <$> toEnum 2013 <*> toEnum 8 <*> toEnum 2)
                <*> (DT.Time <$> toEnum 14 <*> toEnum 34 <*> toEnum 0 <*> toEnum 0)
              endAt = DT.DateTime
                <$> (DT.canonicalDate <$> toEnum 2013 <*> toEnum 8 <*> toEnum 2)
                <*> (DT.Time <$> toEnum 15 <*> toEnum 4 <*> toEnum 0 <*> toEnum 0)
              desiredOutput =
                ( \s e ->
                    [ { summary: "Hey look! An example event!"
                      , startAt: { date: s, tz: Just "America/New_York" }
                      , endAt: { date: e, tz: Just "America/New_York" }
                      , description: Just " Do something in NY."
                      , uid: "7014-1567468800-1567555199@peterbraden@peterbraden.co.uk"
                      , status: Just Confirmed
                      , location: Just "1000 Broadway Ave., Brooklyn"
                      }
                    ]
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

          event =
            ( \s e ->
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

          event =
            ( \s e ->
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

cleaningSpecs :: forall m g. Monad m => MonadThrow Error g => SpecT g Unit m Unit
cleaningSpecs = do
  describe "Airbnb.Cleaning" do
    describe "scheduleFromGuestStays" do
      let
        apartment = Apartment "Greatest Flat"
        link = "https://example.org/borigua"
        mkGuestStay = \from to -> Maybe.maybe (throwError (error "Could not build GuestStay")) pure $ do
          fromDate <- from
          toDate <- to

          pure
            { last4Digits: "1234"
            , apartment
            , fromDate
            , toDate
            , link
            }

        mkDate y m d = DT.canonicalDate <$> toEnum y <*> toEnum m <*> toEnum d
        mkCleaningDate y m d = DT.DateTime
          <$> mkDate y m d
          <*> (DT.Time <$> toEnum 13 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0)

      describe "when dealing with the first stay" do
        it "creates a CleaningWindow spanning a day right before it" do
          guestStay <- mkGuestStay (mkDate 2022 10 1) (mkDate 2022 10 6)

          let
            apartmentStays = Map.fromFoldable [ apartment /\ [ guestStay ] ]
            cleaningSchedule = Cleaning.scheduleFromGuestStays apartmentStays
            firstWindow = Array.head =<< Map.lookup apartment cleaningSchedule

          expectedWindow <- Maybe.maybe (throwError (error "Could not build CleaningWindow")) pure $ do
            fromDate <- mkCleaningDate 2022 9 30
            toDate <- mkCleaningDate 2022 10 1

            pure
              { stay: guestStay
              , from: fromDate
              , to: toDate
              , weekend: PartialWeekend
              }

          (_.weekend <<< unwrap <$> firstWindow) `shouldEqual` Just expectedWindow.weekend

      describe "after the first stay" do
        it "creates CleaningWindows considering every stay" do
          firstStay <- mkGuestStay (mkDate 2022 10 1) (mkDate 2022 10 6)
          secondStay <- mkGuestStay (mkDate 2022 10 8) (mkDate 2022 10 11)
          thirdStay <- mkGuestStay (mkDate 2022 10 15) (mkDate 2022 10 25)

          let
            apartmentStays = Map.fromFoldable [ apartment /\ [ firstStay, secondStay, thirdStay ] ]
            cleaningSchedule = Cleaning.scheduleFromGuestStays apartmentStays
            apartmentSchedule = Map.lookup apartment cleaningSchedule

          expectedWindows <- Maybe.maybe (throwError (error "Could not build CleaningWindow spans")) pure $
            sequence
              [ sequence [ mkCleaningDate 2022 9 30, mkCleaningDate 2022 10 1 ]
              , sequence [ mkCleaningDate 2022 10 6, mkCleaningDate 2022 10 8 ]
              , sequence [ mkCleaningDate 2022 10 11, mkCleaningDate 2022 10 15 ]
              ]

          (Array.length <$> apartmentSchedule) `shouldEqual` Just (Array.length expectedWindows)
          ((map (\(CleaningWindow { from, to }) -> [ from, to ])) <$> apartmentSchedule) `shouldEqual` Just expectedWindows
