module Airbnbeast.Html where

import Prelude hiding (div)

import Airbnbeast.Availability (Apartment(..), GuestStay)
import Airbnbeast.Cleaning (CleaningWeekend(..), CleaningWindow(..))
import Data.Array as Array
import Data.DateTime as DateTime
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
    <>
      tag "style" [] cssStyles

body :: HtmlString -> HtmlString
body content = tag "body" [] content

h1 :: HtmlString -> HtmlString
h1 content = tag "h1" [] content

h2 :: HtmlString -> HtmlString
h2 content = tag "h2" [] content

table :: Array String -> HtmlString -> HtmlString
table attrs content = tag "table" attrs content

tr :: Array String -> HtmlString -> HtmlString
tr attrs content = tag "tr" attrs content

th :: HtmlString -> HtmlString
th content = tag "th" [] content

td :: Array String -> HtmlString -> HtmlString
td attrs content = tag "td" attrs content

div :: Array String -> HtmlString -> HtmlString
div attrs content = tag "div" attrs content

span :: Array String -> HtmlString -> HtmlString
span attrs content = tag "span" attrs content

cssStyles :: HtmlString
cssStyles =
  """
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
    line-height: 1.6;
    color: #333;
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
    background-color: #f5f5f5;
  }
  
  h1 {
    color: #2c3e50;
    text-align: center;
    margin-bottom: 30px;
  }
  
  h2 {
    color: #34495e;
    border-bottom: 2px solid #3498db;
    padding-bottom: 10px;
    margin-top: 30px;
  }
  
  .apartment-section {
    background: white;
    border-radius: 8px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  table {
    width: 100%;
    border-collapse: collapse;
    margin-top: 15px;
  }
  
  th, td {
    padding: 12px;
    text-align: left;
    border-bottom: 1px solid #ddd;
  }
  
  th {
    background-color: #3498db;
    color: white;
    font-weight: 600;
  }
  
  .keypad-code {
    font-family: 'Courier New', monospace;
    font-size: 1.2em;
    font-weight: bold;
    color: #e74c3c;
    background-color: #f8f9fa;
    padding: 4px 8px;
    border-radius: 4px;
    border: 1px solid #dee2e6;
  }
  
  .weekend-full {
    background-color: #e8f5e8;
    color: #2d5a2d;
  }
  
  .weekend-partial {
    background-color: #fff3cd;
    color: #856404;
  }
  
  .weekend-none {
    background-color: #f8f9fa;
    color: #495057;
  }
  
  .date-range {
    font-weight: 500;
    color: #2c3e50;
  }
  
  .no-schedule {
    text-align: center;
    color: #6c757d;
    font-style: italic;
    padding: 40px;
  }
  
  @media (max-width: 768px) {
    body {
      padding: 10px;
    }
    
    table {
      font-size: 0.9em;
    }
    
    th, td {
      padding: 8px;
    }
    
    .keypad-code {
      font-size: 1em;
    }
  }
"""

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
weekendClass AllWeekend = "weekend-full"
weekendClass PartialWeekend = "weekend-partial"
weekendClass NoWeekend = "weekend-none"

weekendText :: CleaningWeekend -> HtmlString
weekendText AllWeekend = "Full Weekend"
weekendText PartialWeekend = "Partial Weekend"
weekendText NoWeekend = "Weekday Only"

cleaningWindowRow :: CleaningWindow -> HtmlString
cleaningWindowRow (CleaningWindow { from, to, weekend, stay }) =
  tr [ attr "class" (weekendClass weekend) ] $
    td [] (span [ attr "class" "date-range" ] (formatDate from <> " → " <> formatDate to))
      <> td [] (span [ attr "class" "keypad-code" ] stay.last4Digits)
      <> td [] (weekendText weekend)
      <>
        td [] (tag "a" [ attr "href" stay.link, attr "target" "_blank" ] "View Reservation")

apartmentSection :: Apartment -> Array CleaningWindow -> HtmlString
apartmentSection (Apartment name) windows =
  div [ attr "class" "apartment-section" ] $
    h2 name <>
      if Array.null windows then div [ attr "class" "no-schedule" ] "No cleaning windows scheduled"
      else table [ attr "class" "cleaning-table" ] $
        tr [] (th "Cleaning Window" <> th "Keypad Code" <> th "Weekend" <> th "Reservation") <>
          Array.foldMap cleaningWindowRow windows

cleaningSchedulePage :: Map Apartment (Array CleaningWindow) -> HtmlString
cleaningSchedulePage schedule =
  html $
    head "Cleaning Schedule - Airbnbeast" <>
      body
        ( h1 "🧹 Cleaning Schedule" <>
            if Map.isEmpty schedule then div [ attr "class" "no-schedule" ] "No cleaning schedule available"
            else Array.foldMap (\(apartment /\ windows) -> apartmentSection apartment windows)
              (Map.toUnfoldable schedule :: Array _)
        )

apartmentPage :: Apartment -> Array CleaningWindow -> HtmlString
apartmentPage apartment@(Apartment name) windows =
  html $
    head ("Cleaning Schedule - " <> name) <>
      body
        ( h1 ("🧹 " <> name <> " Cleaning Schedule") <>
            apartmentSection apartment windows
        )

indexPage :: HtmlString
indexPage =
  html $
    head "Airbnbeast - Home" <>
      body
        ( h1 "🏠 Airbnbeast" <>
            div [ attr "class" "apartment-section" ]
              ( h2 "Available Pages" <>
                  tag "ul" []
                    ( tag "li" [] (tag "a" [ attr "href" "/schedule" ] "Full Cleaning Schedule")
                        <> tag "li" [] (tag "a" [ attr "href" "/apartment/gloria" ] "Glória Apartment")
                        <>
                          tag "li" [] (tag "a" [ attr "href" "/apartment/santa" ] "Santa Apartment")
                    )
              )
        )