module Airbnbeast.Html where

import Prelude hiding (div)

import Airbnbeast.Availability (Apartment(..))
import Airbnbeast.Cleaning (CleaningWeekend(..), CleaningWindow(..))
import Data.Array as Array
import Data.DateTime as DateTime
import Data.Enum (fromEnum)
import Data.Map (Map)
import Data.Map as Map
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
formatDate dt =
  let
    date = DateTime.date dt
    year = show $ fromEnum $ DateTime.year date
    month = show $ fromEnum $ DateTime.month date
    day = show $ fromEnum $ DateTime.day date
    weekday = show $ DateTime.weekday date
  in
    year <> "-" <> month <> "-" <> day <> " (" <> weekday <> ")"

weekendClass :: CleaningWeekend -> String
weekendClass AllWeekend = "bg-green-50"
weekendClass PartialWeekend = "bg-yellow-50"
weekendClass NoWeekend = "bg-gray-50"

weekendText :: CleaningWeekend -> HtmlString
weekendText AllWeekend = "Full Weekend"
weekendText PartialWeekend = "Partial Weekend"
weekendText NoWeekend = "Weekday Only"

cleaningWindowRow :: CleaningWindow -> HtmlString
cleaningWindowRow (CleaningWindow { from, to, weekend, stay }) =
  tr [ attr "class" (weekendClass weekend) ] $
    td [ attr "class" "p-3 border-b border-gray-200" ] (span [ attr "class" "font-medium text-gray-900" ] (formatDate from <> " → " <> formatDate to))
      <> td [ attr "class" "p-3 border-b border-gray-200" ] (span [ attr "class" "font-mono text-lg font-bold text-red-600 bg-gray-50 px-2 py-1 rounded border" ] stay.last4Digits)
      <> td [ attr "class" "p-3 border-b border-gray-200" ] (weekendText weekend)
      <> td [ attr "class" "p-3 border-b border-gray-200" ] (tag "a" [ attr "href" stay.link, attr "target" "_blank", attr "class" "text-blue-600 hover:text-blue-800 underline" ] "View Reservation")

apartmentSection :: Apartment -> Array CleaningWindow -> HtmlString
apartmentSection (Apartment name) windows =
  div [ attr "class" "bg-white rounded-lg p-6 mb-6 shadow-sm border border-gray-200" ] $
    h2 [ attr "class" "text-2xl font-semibold text-gray-800 mb-4 pb-2 border-b-2 border-blue-500" ] name <>
      if Array.null windows then div [ attr "class" "text-center text-gray-500 italic py-10" ] "No cleaning windows scheduled"
      else table [ attr "class" "w-full border-collapse" ] $
        tr [ attr "class" "bg-blue-500 text-white" ] (th [ attr "class" "p-3 text-left font-semibold" ] "Cleaning Window" <> th [ attr "class" "p-3 text-left font-semibold" ] "Keypad Code" <> th [ attr "class" "p-3 text-left font-semibold" ] "Weekend" <> th [ attr "class" "p-3 text-left font-semibold" ] "Reservation") <>
          Array.foldMap cleaningWindowRow windows

cleaningSchedulePage :: Map Apartment (Array CleaningWindow) -> HtmlString
cleaningSchedulePage schedule =
  html $
    head "Cleaning Schedule - Airbnbeast" <>
      body
        ( div [ attr "class" "max-w-6xl mx-auto p-6" ]
          ( h1 [ attr "class" "text-4xl font-bold text-center text-gray-800 mb-8" ] "🧹 Cleaning Schedule" <>
              if Map.isEmpty schedule then div [ attr "class" "text-center text-gray-500 italic py-20" ] "No cleaning schedule available"
              else Array.foldMap (\(apartment /\ windows) -> apartmentSection apartment windows)
                (Map.toUnfoldable schedule :: Array _)
          )
        )

apartmentPage :: Apartment -> Array CleaningWindow -> HtmlString
apartmentPage apartment@(Apartment name) windows =
  html $
    head ("Cleaning Schedule - " <> name) <>
      body
        ( div [ attr "class" "max-w-6xl mx-auto p-6" ]
          ( h1 [ attr "class" "text-4xl font-bold text-center text-gray-800 mb-8" ] ("🧹 " <> name <> " Cleaning Schedule") <>
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