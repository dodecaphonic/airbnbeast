module Airbnbeast.Html where

import Prelude hiding (div)

import Airbnbeast.Availability (Apartment(..))
import Airbnbeast.Cleaning (CleaningWindow(..), TimeOfDay(..), TimeBlock(..))
import Airbnbeast.I18n as I18n
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Date as Date
import Data.Enum (fromEnum, enumFromTo)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\))

type HtmlString = String

tag :: String -> Array String -> HtmlString -> HtmlString
tag tagName attrs content =
  "<" <> tagName <> attrsStr <> ">" <> content <> "</" <> tagName <> ">"
  where
  attrsStr = if Array.null attrs then "" else " " <> Array.intercalate " " attrs

voidTag :: String -> Array String -> HtmlString
voidTag tagName attrs =
  "<" <> tagName <> attrsStr <> "/>"
  where
  attrsStr = if Array.null attrs then "" else " " <> Array.intercalate " " attrs

attr :: String -> String -> String
attr name value = name <> "=\"" <> value <> "\""

html :: HtmlString -> HtmlString
html content = "<!DOCTYPE html>" <> tag "html" [ attr "lang" "en" ] content

head :: String -> HtmlString
head title = tag "head" [] $
  tag "meta" [ attr "charset" "utf-8" ] ""
    <> tag "meta" [ attr "name" "viewport", attr "content" "width=device-width, initial-scale=1" ] ""
    <> tag "meta" [ attr "name" "turbo-cache-control", attr "content" "no-cache" ] ""
    <> tag "meta" [ attr "name" "turbo-refresh-method", attr "content" "morph" ] ""
    <> tag "meta" [ attr "name" "turbo-refresh-scroll", attr "content" "preserve" ] ""
    <> tag "title" [] title
    <> tag "link" [ attr "rel" "stylesheet", attr "href" "/tailwind.css" ] ""
    <> tag "script" [ attr "src" "/application.js" ] ""

body :: HtmlString -> HtmlString
body content = tag "body" [ attr "class" "bg-gray-50 min-h-screen" ] content

h1 :: Array String -> HtmlString -> HtmlString
h1 attrs content = tag "h1" attrs content

h2 :: Array String -> HtmlString -> HtmlString
h2 attrs content = tag "h2" attrs content

table :: Array String -> HtmlString -> HtmlString
table attrs content = tag "table" attrs content

tr :: Array String -> HtmlString -> HtmlString
tr attrs content = tag "tr" attrs content

th :: Array String -> HtmlString -> HtmlString
th attrs content = tag "th" attrs content

td :: Array String -> HtmlString -> HtmlString
td attrs content = tag "td" attrs content

div :: Array String -> HtmlString -> HtmlString
div attrs content = tag "div" attrs content

span :: Array String -> HtmlString -> HtmlString
span attrs content = tag "span" attrs content

turboFrame :: String -> HtmlString -> HtmlString
turboFrame frameId content = tag "turbo-frame" [ attr "id" frameId ] content

formatDate :: DateTime.DateTime -> HtmlString
formatDate = I18n.formatDatePt

apartmentToUrl :: Apartment -> String
apartmentToUrl (Apartment "Glória") = "gloria"
apartmentToUrl (Apartment "Santa") = "santa"
apartmentToUrl (Apartment name) = name -- fallback for any other apartments

-- Render the time block grid for a cleaning window
renderTimeBlockGrid :: CleaningWindow -> HtmlString
renderTimeBlockGrid (CleaningWindow { timeBlocks }) =
  let
    timeBlocksArray = NEArray.toArray timeBlocks
    availableCount = Array.length $ Array.filter (\(TimeBlock { available }) -> available) timeBlocksArray
    groupedByDate = groupBlocksByDate timeBlocksArray
  in
    div [ attr "class" "space-y-2" ] $
      div [ attr "class" "text-xs text-gray-600 mb-2 text-center" ] I18n.pt.clickToToggle <>
        Array.foldMap (renderDateBlocks availableCount) groupedByDate
  where
  groupBlocksByDate :: Array TimeBlock -> Array { date :: Date.Date, blocks :: Array TimeBlock }
  groupBlocksByDate blocks =
    let
      grouped = Array.groupBy (\(TimeBlock a) (TimeBlock b) -> a.date == b.date)
        (Array.sortBy (\(TimeBlock a) (TimeBlock b) -> compare a.date b.date) blocks)
    in
      Array.mapMaybe
        ( \group ->
            case NEArray.head group of
              TimeBlock { date } -> Just { date, blocks: NEArray.toArray group }
        )
        grouped

  renderDateBlocks :: Int -> { date :: Date.Date, blocks :: Array TimeBlock } -> HtmlString
  renderDateBlocks availableCount { date, blocks } =
    div [ attr "class" "flex items-center justify-between py-1" ] $
      div [ attr "class" "text-xs font-medium text-gray-700 w-20" ] (formatDateOnly date) <>
        div [ attr "class" "flex gap-1" ] (Array.foldMap (renderTimeBlock availableCount) blocks)

  renderTimeBlock :: Int -> TimeBlock -> HtmlString
  renderTimeBlock availableCount (TimeBlock { date, timeOfDay, available, apartment }) =
    let
      timeLabel = case timeOfDay of
        Morning -> I18n.pt.morning
        Afternoon -> I18n.pt.afternoon

      -- Determine if this block can be toggled
      isDisabled = available && availableCount <= 1 -- Can't disable the last available block

      baseClasses = "text-xs px-2 py-1 rounded transition-colors no-underline "
      statusClasses =
        if available then
          if isDisabled then "bg-gray-50 text-gray-400 cursor-not-allowed"
          else "bg-green-100 text-green-700 hover:bg-green-200 cursor-pointer"
        else "bg-red-100 text-red-700 hover:bg-red-200 line-through cursor-pointer"

      dateStr = show (fromEnum $ Date.year date) <> "-"
        <> (if fromEnum (Date.month date) < 10 then "0" else "")
        <> show (fromEnum $ Date.month date)
        <> "-"
        <> (if fromEnum (Date.day date) < 10 then "0" else "")
        <> show (fromEnum $ Date.day date)

      apartmentName = case apartment of
        Apartment name -> case name of
          "Glória" -> "gloria"
          "Santa" -> "santa"
          _ -> name

      timeOfDayStr = case timeOfDay of
        Morning -> "morning"
        Afternoon -> "afternoon"

      -- Build the RESTful URL: /apartments/:apartment/time-blocks/:date/:timeOfDay
      href = "/apartments/" <> apartmentName <> "/time-blocks/" <> dateStr <> "/" <> timeOfDayStr

      -- Determine HTTP method: POST for enable (when currently unavailable), DELETE for disable (when currently available)
      turboMethod = if available then "delete" else "post"

      linkAttrs =
        if isDisabled then [ attr "class" (baseClasses <> statusClasses) ]
        else
          [ attr "class" (baseClasses <> statusClasses)
          , attr "href" href
          , attr "data-turbo-method" turboMethod
          ]
    in
      tag "a" linkAttrs timeLabel

  formatDateOnly :: Date.Date -> String
  formatDateOnly date =
    let
      day = fromEnum $ Date.day date
      month = fromEnum $ Date.month date
      dayStr = if day < 10 then "0" <> show day else show day
      monthStr = if month < 10 then "0" <> show month else show month
    in
      dayStr <> "/" <> monthStr

-- Calculate the effective date range by trimming unavailable dates from the ends
-- and detecting if there are gaps in availability
calculateEffectiveDateRange
  :: NEArray.NonEmptyArray TimeBlock
  -> DateTime
  -> DateTime
  -> { effectiveRange :: Maybe { from :: DateTime, to :: DateTime }, hasGaps :: Boolean }
calculateEffectiveDateRange timeBlocks originalFrom originalTo =
  let
    timeBlocksArray = NEArray.toArray timeBlocks
    availableBlocks = Array.filter (\(TimeBlock { available }) -> available) timeBlocksArray
    availableDates = Set.fromFoldable $ map (\(TimeBlock { date }) -> date) availableBlocks

    -- Find the earliest and latest available dates
    sortedAvailableDates = Array.sort $ Set.toUnfoldable availableDates

    -- Calculate effective range
    effectiveRange = do
      firstAvailableDate <- Array.head sortedAvailableDates
      lastAvailableDate <- Array.last sortedAvailableDates

      -- Create DateTime objects for the effective range
      effectiveFrom <- createDateTimeFromDate firstAvailableDate originalFrom
      effectiveTo <- createDateTimeFromDate lastAvailableDate originalTo

      pure { from: effectiveFrom, to: effectiveTo }

    -- Check for gaps: if we have an effective range, see if all dates in that range have availability
    hasGaps = case effectiveRange of
      Just { from: effFrom, to: effTo } ->
        let
          effectiveFromDate = DateTime.date effFrom
          effectiveToDate = DateTime.date effTo
          allDatesInRange = enumFromTo effectiveFromDate effectiveToDate
          availableDatesInRange = Array.filter (\date -> Set.member date availableDates) allDatesInRange
        in
          Array.length availableDatesInRange < Array.length allDatesInRange
      Nothing -> false
  in
    { effectiveRange, hasGaps }
  where
  createDateTimeFromDate :: Date.Date -> DateTime -> Maybe DateTime
  createDateTimeFromDate date originalDateTime =
    let
      time = DateTime.time originalDateTime
    in
      pure $ DateTime date time

cleaningWindowCard :: { isFirst :: Boolean, isOpen :: Boolean } -> CleaningWindow -> HtmlString
cleaningWindowCard { isFirst, isOpen } window@(CleaningWindow { from, to, stay, timeBlocks }) =
  let
    -- Generate unique frame ID based on apartment and stay details
    frameId = "cleaning-window-" <> apartmentToUrl stay.apartment <> "-" <> stay.last4Digits

    cardClasses =
      if isFirst then "bg-blue-50 rounded-lg shadow-md border border-blue-200 p-4 hover:shadow-lg transition-shadow"
      else "bg-white rounded-lg shadow-md border border-gray-200 p-4 hover:shadow-lg transition-shadow"

    dateClasses =
      if isFirst then "text-sm text-blue-700 mb-2 text-center"
      else "text-sm text-gray-600 mb-2 text-center"

    codeClasses =
      if isFirst then "text-3xl font-bold text-blue-800 mb-3 text-center font-mono"
      else "text-3xl font-bold text-blue-600 mb-3 text-center font-mono"

    linkClasses =
      if isFirst then "text-blue-700 hover:text-blue-900 text-sm"
      else "text-blue-600 hover:text-blue-800 text-sm"

    buttonClasses =
      if isFirst then "text-xs bg-blue-100 hover:bg-blue-200 text-blue-700 px-3 py-1 rounded-full transition-colors"
      else "text-xs bg-gray-100 hover:bg-gray-200 text-gray-700 px-3 py-1 rounded-full transition-colors"

    gridClasses =
      if isFirst then "hidden mt-4 pt-4 border-t border-blue-300"
      else "hidden mt-4 pt-4 border-t border-gray-200"

    -- Calculate effective date range based on available TimeBlocks
    { effectiveRange, hasGaps } = calculateEffectiveDateRange timeBlocks from to

    dateRangeDisplay = case effectiveRange of
      Just { from: effectiveFrom, to: effectiveTo } ->
        let
          fromDate = DateTime.date effectiveFrom
          toDate = DateTime.date effectiveTo
          rangeText =
            if fromDate == toDate then formatDate effectiveFrom
            else formatDate effectiveFrom <> " → " <> formatDate effectiveTo
        in
          div [ attr "class" dateClasses ] rangeText <>
            if hasGaps then
              div [ attr "class" "text-xs text-red-500 mt-1 mb-4 text-center" ] I18n.pt.spottyRange
            else ""
      Nothing ->
        div [ attr "class" dateClasses ] (formatDate from <> " → " <> formatDate to)

    cardContent =
      div
        [ attr "class" cardClasses
        , attr "data-controller" "cleaning-window"
        , attr "data-cleaning-window-adjust-text-value" I18n.pt.adjustPeriods
        , attr "data-cleaning-window-hide-text-value" I18n.pt.hidePeriods
        , attr "data-cleaning-window-time-blocks-visible-value" (show isOpen)
        ] $
        dateRangeDisplay
          <> div [ attr "class" codeClasses ] stay.last4Digits
          <> div [ attr "class" "text-center mb-3" ] (tag "a" [ attr "href" stay.link, attr "target" "_blank", attr "class" linkClasses ] I18n.pt.viewReservation)
          <> div [ attr "class" "text-center" ]
            ( tag "button"
                [ attr "class" buttonClasses
                , attr "data-cleaning-window-target" "button"
                , attr "data-action" "click->cleaning-window#toggle"
                ]
                I18n.pt.adjustPeriods
            )
          <> div [ attr "data-cleaning-window-target" "grid", attr "class" gridClasses ]
            (renderTimeBlockGrid window)
  in
    turboFrame frameId cardContent

apartmentSection :: Apartment -> Array CleaningWindow -> HtmlString
apartmentSection apartment@(Apartment name) windows =
  div [ attr "class" "mb-8" ] $
    h2 [ attr "class" "text-2xl font-semibold mb-4 pb-2 border-b-2 border-blue-500" ]
      (tag "a" [ attr "href" ("/apartment/" <> apartmentToUrl apartment), attr "class" "text-gray-800 hover:text-blue-600 transition-colors no-underline" ] name) <>
      if Array.null windows then div [ attr "class" "text-center text-gray-500 italic py-10" ] I18n.pt.noCleaningWindows
      else div [ attr "class" "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4" ] $
        renderWindowCards windows
  where
  renderWindowCards :: Array CleaningWindow -> HtmlString
  renderWindowCards ws =
    Array.fold $ Array.mapWithIndex (\index window -> cleaningWindowCard { isFirst: index == 0, isOpen: false } window) ws

cleaningSchedulePage :: Map Apartment (Array CleaningWindow) -> HtmlString
cleaningSchedulePage schedule =
  html $
    head I18n.pt.pageTitle <>
      body
        ( div [ attr "class" "max-w-6xl mx-auto p-6" ]
            ( h1 [ attr "class" "text-4xl font-bold text-center text-gray-800 mb-8" ] I18n.pt.cleaningSchedule <>
                if Map.isEmpty schedule then div [ attr "class" "text-center text-gray-500 italic py-20" ] I18n.pt.noCleaningWindows
                else Array.foldMap (\(apartment /\ windows) -> apartmentSection apartment windows)
                  (Map.toUnfoldable schedule :: Array _)
            )
        )

apartmentPage :: Apartment -> Array CleaningWindow -> HtmlString
apartmentPage apartment@(Apartment name) windows =
  html $
    head (I18n.pt.apartmentSchedule name) <>
      body
        ( div [ attr "class" "max-w-6xl mx-auto p-6" ]
            ( h1 [ attr "class" "text-4xl font-bold text-center text-gray-800 mb-8" ] (I18n.pt.apartmentSchedule name) <>
                apartmentSection apartment windows
            )
        )

indexPage :: HtmlString
indexPage =
  html $
    head "Airbnbeast - Home" <>
      body
        ( div [ attr "class" "max-w-4xl mx-auto p-6" ]
            ( h1 [ attr "class" "text-4xl font-bold text-center text-gray-800 mb-8" ] "🏠 Airbnbeast" <>
                div [ attr "class" "bg-white rounded-lg p-6 shadow-sm border border-gray-200" ]
                  ( h2 [ attr "class" "text-2xl font-semibold text-gray-800 mb-4" ] "Available Pages" <>
                      tag "ul" [ attr "class" "space-y-3" ]
                        ( tag "li" [] (tag "a" [ attr "href" "/schedule", attr "class" "block p-4 bg-blue-50 hover:bg-blue-100 rounded-lg border border-blue-200 text-blue-700 hover:text-blue-800 font-medium transition-colors" ] "📋 Full Cleaning Schedule")
                            <> tag "li" [] (tag "a" [ attr "href" "/apartment/gloria", attr "class" "block p-4 bg-green-50 hover:bg-green-100 rounded-lg border border-green-200 text-green-700 hover:text-green-800 font-medium transition-colors" ] "🏠 Glória Apartment")
                            <> tag "li" [] (tag "a" [ attr "href" "/apartment/santa", attr "class" "block p-4 bg-purple-50 hover:bg-purple-100 rounded-lg border border-purple-200 text-purple-700 hover:text-purple-800 font-medium transition-colors" ] "🏠 Santa Apartment")
                        )
                  )
            )
        )