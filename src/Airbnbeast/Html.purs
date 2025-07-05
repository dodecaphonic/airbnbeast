module Airbnbeast.Html where

import Prelude hiding (div)

import Airbnbeast.Availability (Apartment(..))
import Airbnbeast.Cleaning (CleaningWindow(..), TimeOfDay(..), TimeBlock(..), cleaningWindowToTimeBlocks, timeBlocksToDateRange)
import Airbnbeast.I18n as I18n
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.DateTime as DateTime
import Data.Date as Date
import Data.Enum (fromEnum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
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


formatDate :: DateTime.DateTime -> HtmlString
formatDate = I18n.formatDatePt

apartmentToUrl :: Apartment -> String
apartmentToUrl (Apartment "Glória") = "gloria"
apartmentToUrl (Apartment "Santa") = "santa"
apartmentToUrl (Apartment name) = name -- fallback for any other apartments

-- Process cleaning windows through time block system
-- For now, this just demonstrates the round-trip conversion
-- Later we'll add manual override functionality here
processCleaningWindow :: CleaningWindow -> Maybe CleaningWindow
processCleaningWindow window@(CleaningWindow { stay }) = 
  let
    timeBlocks = cleaningWindowToTimeBlocks window
    -- TODO: Apply manual overrides here by setting available = false for blocked periods
  in
    case timeBlocksToDateRange timeBlocks of
      Just { from, to } -> 
        Just $ CleaningWindow
          { from
          , to
          , weekend: (unwrap window).weekend -- Preserve weekend info for now
          , stay
          }
      Nothing -> Nothing

-- Render the time block grid for a cleaning window
renderTimeBlockGrid :: CleaningWindow -> HtmlString
renderTimeBlockGrid window =
  let
    timeBlocks = cleaningWindowToTimeBlocks window
    groupedByDate = groupBlocksByDate timeBlocks
  in
    div [ attr "class" "space-y-2" ] $
      div [ attr "class" "text-xs text-gray-600 mb-2 text-center" ] I18n.pt.clickToToggle <>
      Array.foldMap renderDateBlocks groupedByDate
  where
  groupBlocksByDate :: Array TimeBlock -> Array { date :: Date.Date, blocks :: Array TimeBlock }
  groupBlocksByDate blocks =
    let
      grouped = Array.groupBy (\(TimeBlock a) (TimeBlock b) -> a.date == b.date) 
                  (Array.sortBy (\(TimeBlock a) (TimeBlock b) -> compare a.date b.date) blocks)
    in
      Array.mapMaybe (\group -> 
        case NEArray.head group of
          TimeBlock { date } -> Just { date, blocks: NEArray.toArray group }
      ) grouped
  
  renderDateBlocks :: { date :: Date.Date, blocks :: Array TimeBlock } -> HtmlString
  renderDateBlocks { date, blocks } =
    div [ attr "class" "flex items-center justify-between py-1" ] $
      div [ attr "class" "text-xs font-medium text-gray-700 w-20" ] (formatDateOnly date) <>
      div [ attr "class" "flex gap-1" ] (Array.foldMap renderTimeBlock blocks)
  
  renderTimeBlock :: TimeBlock -> HtmlString
  renderTimeBlock (TimeBlock { date, timeOfDay, available, apartment }) =
    let
      blockId = "block-" <> apartmentName <> "-" <> show date <> "-" <> show timeOfDay
      timeLabel = case timeOfDay of
        Morning -> I18n.pt.morning
        Afternoon -> I18n.pt.afternoon
      baseClasses = "text-xs px-2 py-1 rounded cursor-pointer transition-colors "
      statusClasses = if available 
        then "bg-green-100 text-green-700 hover:bg-green-200"
        else "bg-red-100 text-red-700 hover:bg-red-200 line-through"
      dateStr = show (fromEnum $ Date.year date) <> "-" <> 
                (if fromEnum (Date.month date) < 10 then "0" else "") <> show (fromEnum $ Date.month date) <> "-" <> 
                (if fromEnum (Date.day date) < 10 then "0" else "") <> show (fromEnum $ Date.day date)
      apartmentName = case apartment of
        Apartment name -> case name of
          "Glória" -> "gloria"
          "Santa" -> "santa" 
          _ -> name
    in
      tag "button"
        [ attr "class" (baseClasses <> statusClasses)
        , attr "id" blockId
        , attr "data-controller" "time-block"
        , attr "data-time-block-apartment-value" apartmentName
        , attr "data-time-block-date-value" dateStr
        , attr "data-time-block-time-of-day-value" (show timeOfDay)
        , attr "data-action" "click->time-block#toggleBlock"
        ]
        timeLabel
  
  formatDateOnly :: Date.Date -> String
  formatDateOnly date =
    let
      day = fromEnum $ Date.day date
      month = fromEnum $ Date.month date
      dayStr = if day < 10 then "0" <> show day else show day
      monthStr = if month < 10 then "0" <> show month else show month
    in
      dayStr <> "/" <> monthStr

cleaningWindowCard :: CleaningWindow -> HtmlString
cleaningWindowCard window@(CleaningWindow { from, to, stay }) =
    div [ attr "class" "bg-white rounded-lg shadow-md border border-gray-200 p-4 hover:shadow-lg transition-shadow"
        , attr "data-controller" "cleaning-window"
        , attr "data-cleaning-window-adjust-text-value" I18n.pt.adjustPeriods
        , attr "data-cleaning-window-hide-text-value" I18n.pt.hidePeriods
        ] $
      div [ attr "class" "text-sm text-gray-600 mb-2 text-center" ] (formatDate from <> " → " <> formatDate to)
        <> div [ attr "class" "text-3xl font-bold text-blue-600 mb-3 text-center font-mono" ] stay.last4Digits
        <> div [ attr "class" "text-center mb-3" ] (tag "a" [ attr "href" stay.link, attr "target" "_blank", attr "class" "text-blue-600 hover:text-blue-800 text-sm" ] I18n.pt.viewReservation)
        <> div [ attr "class" "text-center" ] 
          (tag "button" 
            [ attr "class" "text-xs bg-gray-100 hover:bg-gray-200 text-gray-700 px-3 py-1 rounded-full transition-colors"
            , attr "data-cleaning-window-target" "button"
            , attr "data-action" "click->cleaning-window#toggle"
            ] 
            I18n.pt.adjustPeriods
          )
        <> div [ attr "data-cleaning-window-target" "grid", attr "class" "hidden mt-4 pt-4 border-t border-gray-200" ] 
          (renderTimeBlockGrid window)

cleaningWindowCardFirst :: CleaningWindow -> HtmlString
cleaningWindowCardFirst window@(CleaningWindow { from, to, stay }) =
    div [ attr "class" "bg-blue-50 rounded-lg shadow-md border border-blue-200 p-4 hover:shadow-lg transition-shadow"
        , attr "data-controller" "cleaning-window"
        , attr "data-cleaning-window-adjust-text-value" I18n.pt.adjustPeriods
        , attr "data-cleaning-window-hide-text-value" I18n.pt.hidePeriods
        ] $
      div [ attr "class" "text-sm text-blue-700 mb-2 text-center" ] (formatDate from <> " → " <> formatDate to)
        <> div [ attr "class" "text-3xl font-bold text-blue-800 mb-3 text-center font-mono" ] stay.last4Digits
        <> div [ attr "class" "text-center mb-3" ] (tag "a" [ attr "href" stay.link, attr "target" "_blank", attr "class" "text-blue-700 hover:text-blue-900 text-sm" ] I18n.pt.viewReservation)
        <> div [ attr "class" "text-center" ] 
          (tag "button" 
            [ attr "class" "text-xs bg-blue-100 hover:bg-blue-200 text-blue-700 px-3 py-1 rounded-full transition-colors"
            , attr "data-cleaning-window-target" "button"
            , attr "data-action" "click->cleaning-window#toggle"
            ] 
            I18n.pt.adjustPeriods
          )
        <> div [ attr "data-cleaning-window-target" "grid", attr "class" "hidden mt-4 pt-4 border-t border-blue-300" ] 
          (renderTimeBlockGrid window)

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
  renderWindowCards [] = ""
  renderWindowCards ws = 
    let
      -- Process all windows through time block system
      processedWindows = Array.mapMaybe processCleaningWindow ws
    in
      case Array.uncons processedWindows of
        Just { head: first, tail: rest } -> cleaningWindowCardFirst first <> Array.foldMap cleaningWindowCard rest
        Nothing -> ""

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