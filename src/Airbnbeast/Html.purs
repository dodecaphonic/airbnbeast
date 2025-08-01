module Airbnbeast.Html where

import Prelude hiding (div)

import Airbnbeast.Availability (Apartment(..), GuestStay, ReservationSource(..))
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
renderTimeBlockGrid :: Boolean -> CleaningWindow -> HtmlString
renderTimeBlockGrid isAdmin (CleaningWindow { timeBlocks }) =
  let
    timeBlocksArray = NEArray.toArray timeBlocks
    -- Filter time blocks for non-admin users to show only available ones
    filteredTimeBlocks =
      if isAdmin then timeBlocksArray
      else Array.filter (\(TimeBlock { available }) -> available) timeBlocksArray
    availableCount = Array.length $ Array.filter (\(TimeBlock { available }) -> available) timeBlocksArray
    groupedByDate = groupBlocksByDate filteredTimeBlocks
  in
    if Array.null filteredTimeBlocks then
      div [ attr "class" "text-center text-gray-500 italic py-4" ] I18n.pt.noTimeBlocksAvailable
    else
      div [ attr "class" "space-y-2" ] $
        (if isAdmin then div [ attr "class" "text-xs text-gray-600 mb-2 text-center" ] I18n.pt.clickToToggle else "") <>
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
        div [ attr "class" "flex gap-1" ] (Array.foldMap (renderTimeBlock isAdmin availableCount) blocks)

  renderTimeBlock :: Boolean -> Int -> TimeBlock -> HtmlString
  renderTimeBlock isAdmin availableCount (TimeBlock { date, timeOfDay, available, apartment }) =
    let
      timeLabel = case timeOfDay of
        Morning -> I18n.pt.morning
        Afternoon -> I18n.pt.afternoon

      -- Determine if this block can be toggled
      -- For admins: can't disable the last available block
      -- For non-admins: blocks look normal but have no functionality
      isDisabled = isAdmin && available && availableCount <= 1

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
        if isDisabled || not isAdmin then
          -- Disabled blocks (last available) or non-admin users: no functionality
          [ attr "class" (baseClasses <> statusClasses) ]
        else
          -- Admin users with functional blocks
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

cleaningWindowCard :: { isFirst :: Boolean, isOpen :: Boolean, isAdmin :: Boolean } -> CleaningWindow -> HtmlString
cleaningWindowCard { isFirst, isOpen, isAdmin } window@(CleaningWindow { from, to, stay, timeBlocks }) =
  let
    -- Generate unique frame ID based on apartment and stay details
    frameId = "cleaning-window-" <> stay.id

    cardClasses =
      if isFirst then "relative bg-blue-50 rounded-lg shadow-md border border-blue-200 p-4 hover:shadow-lg transition-shadow"
      else "relative bg-white rounded-lg shadow-md border border-gray-200 p-4 hover:shadow-lg transition-shadow"

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
      if not isAdmin then
        -- Non-admin users: always show the grid
        if isFirst then "mt-4 pt-4 border-t border-blue-300"
        else "mt-4 pt-4 border-t border-gray-200"
      else
        -- Admin users: hide by default (controlled by toggle)
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

    markDoneUrl = "/admin/cleaning-windows/" <> stay.id <> "/mark-done"

    cardContent =
      div
        [ attr "class" cardClasses
        , attr "data-controller" "cleaning-window"
        , attr "data-cleaning-window-adjust-text-value" I18n.pt.adjustPeriods
        , attr "data-cleaning-window-hide-text-value" I18n.pt.hidePeriods
        , attr "data-cleaning-window-time-blocks-visible-value" (show isOpen)
        ] $
        ( if isAdmin then
            div [ attr "class" "absolute top-4 right-4" ]
              ( tag "a"
                  [ attr "href" markDoneUrl
                  , attr "class" "text-green-600 hover:text-green-800 text-xl"
                  , attr "data-turbo-method" "post"
                  , attr "data-turbo-confirm" I18n.pt.markCleaningDoneConfirm
                  , attr "title" "Marcar limpeza como feita"
                  ]
                  "✅"
              )
          else ""
        )
          <> dateRangeDisplay
          <> div [ attr "class" codeClasses ] stay.last4Digits
          <> div [ attr "class" "text-center mb-3" ] (tag "a" [ attr "href" stay.link, attr "target" "_blank", attr "class" linkClasses ] I18n.pt.viewReservation)
          <>
            ( if isAdmin then
                div [ attr "class" "text-center" ]
                  ( tag "button"
                      [ attr "class" buttonClasses
                      , attr "data-cleaning-window-target" "button"
                      , attr "data-action" "click->cleaning-window#toggle"
                      ]
                      I18n.pt.adjustPeriods
                  )
              else ""
            )
          <> div [ attr "data-cleaning-window-target" "grid", attr "class" gridClasses ]
            (renderTimeBlockGrid isAdmin window)
  in
    turboFrame frameId cardContent

apartmentSection :: Boolean -> Apartment -> Array CleaningWindow -> HtmlString
apartmentSection isAdmin apartment@(Apartment name) windows =
  div [ attr "class" "mb-8" ] $
    h2 [ attr "class" "text-2xl font-semibold mb-4 pb-2 border-b-2 border-blue-500" ]
      (tag "a" [ attr "href" ("/apartment/" <> apartmentToUrl apartment), attr "class" "text-gray-800 hover:text-blue-600 transition-colors no-underline" ] name) <>
      if Array.null windows then div [ attr "class" "text-center text-gray-500 italic py-10" ] I18n.pt.noCleaningWindows
      else div [ attr "class" "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4" ] $
        renderWindowCards windows
  where
  renderWindowCards :: Array CleaningWindow -> HtmlString
  renderWindowCards ws =
    Array.fold $ Array.mapWithIndex (\index window -> cleaningWindowCard { isFirst: index == 0, isOpen: not isAdmin, isAdmin } window) ws

cleaningSchedulePage :: Boolean -> Map Apartment (Array CleaningWindow) -> HtmlString
cleaningSchedulePage isAdmin schedule =
  html $
    head I18n.pt.pageTitle <>
      body
        ( div [ attr "class" "max-w-6xl mx-auto p-6" ]
            ( h1 [ attr "class" "text-4xl font-bold text-center text-gray-800 mb-8" ] I18n.pt.cleaningSchedule <>
                if Map.isEmpty schedule then div [ attr "class" "text-center text-gray-500 italic py-20" ] I18n.pt.noCleaningWindows
                else Array.foldMap (\(apartment /\ windows) -> apartmentSection isAdmin apartment windows)
                  (Map.toUnfoldable schedule :: Array _)
            )
        )

apartmentPage :: Boolean -> Apartment -> Array CleaningWindow -> HtmlString
apartmentPage isAdmin apartment@(Apartment name) windows =
  html $
    head (I18n.pt.apartmentSchedule name) <>
      body
        ( div [ attr "class" "max-w-6xl mx-auto p-6" ]
            ( h1 [ attr "class" "text-4xl font-bold text-center text-gray-800 mb-8" ] (I18n.pt.apartmentSchedule name) <>
                apartmentSection isAdmin apartment windows
            )
        )

indexPage :: Boolean -> HtmlString
indexPage isAdmin =
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
                            <> (if isAdmin then tag "li" [] (tag "a" [ attr "href" "/admin/guest-stays", attr "class" "block p-4 bg-orange-50 hover:bg-orange-100 rounded-lg border border-orange-200 text-orange-700 hover:text-orange-800 font-medium transition-colors" ] "⚙️ Manage Guest Stays (Admin)") else "")
                        )
                  )
            )
        )

loginPage :: Maybe String -> HtmlString
loginPage errorMsg =
  html $
    head I18n.pt.loginPageTitle <>
      body
        ( div [ attr "class" "min-h-screen w-full flex items-center justify-center bg-gradient-to-br from-blue-50 to-indigo-100 py-12 px-4 sm:px-6 lg:px-8" ]
            ( div [ attr "class" "w-full sm:max-w-md" ]
                ( div [ attr "class" "bg-white shadow-2xl rounded-2xl px-8 py-10 space-y-8" ]
                    ( div [ attr "class" "text-center" ]
                        ( h1 [ attr "class" "text-5xl font-bold text-gray-900 mb-3" ] "🏠"
                            <> div [ attr "class" "text-3xl font-bold text-blue-600 mb-2" ] "Airbnbeast"
                            <>
                              h2 [ attr "class" "text-lg text-gray-600 font-medium" ] I18n.pt.login
                        )
                        <>
                          ( case errorMsg of
                              Just err -> div [ attr "class" "mt-6 p-4 bg-red-50 border border-red-200 text-red-700 rounded-lg text-sm" ] err
                              Nothing -> ""
                          )
                        <>
                          tag "form" [ attr "class" "mt-8 space-y-6", attr "action" "/auth/login", attr "method" "POST" ]
                            ( div [ attr "class" "space-y-5" ]
                                ( div []
                                    ( tag "label" [ attr "for" "username", attr "class" "block text-sm font-semibold text-gray-700 mb-2" ] I18n.pt.username <>
                                        tag "input" [ attr "id" "username", attr "name" "username", attr "type" "text", attr "required" "true", attr "class" "block w-full px-4 py-3 border border-gray-300 rounded-lg shadow-sm placeholder-gray-400 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-colors" ] ""
                                    ) <>
                                    div []
                                      ( tag "label" [ attr "for" "password", attr "class" "block text-sm font-semibold text-gray-700 mb-2" ] I18n.pt.password <>
                                          tag "input" [ attr "id" "password", attr "name" "password", attr "type" "password", attr "required" "true", attr "class" "block w-full px-4 py-3 border border-gray-300 rounded-lg shadow-sm placeholder-gray-400 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-colors" ] ""
                                      )
                                ) <>
                                div []
                                  ( tag "button" [ attr "type" "submit", attr "class" "w-full flex justify-center py-3 px-4 border border-transparent rounded-lg shadow-sm text-base font-semibold text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 transition-colors" ] I18n.pt.loginButton
                                  )
                            )
                    )
                )
            )
        )

guestStaysListPage :: Boolean -> Array GuestStay -> HtmlString
guestStaysListPage isAdmin guestStays = guestStaysListPageWithError isAdmin guestStays Nothing

guestStaysListPageWithError :: Boolean -> Array GuestStay -> Maybe String -> HtmlString
guestStaysListPageWithError isAdmin guestStays errorMsg =
  html $
    head "Admin - Guest Stays Management" <>
      body
        ( div [ attr "class" "max-w-6xl mx-auto p-6" ]
            ( h1 [ attr "class" "text-4xl font-bold text-center text-gray-800 mb-8" ] "Guest Stays Management"
                <>
                  ( case errorMsg of
                      Just msg -> div [ attr "class" "mb-6 p-4 bg-red-50 border border-red-200 rounded-lg" ]
                        ( div [ attr "class" "flex" ]
                            ( div [ attr "class" "flex-shrink-0" ]
                                ( tag "svg" [ attr "class" "h-5 w-5 text-red-400", attr "viewBox" "0 0 20 20", attr "fill" "currentColor" ]
                                    (tag "path" [ attr "fill-rule" "evenodd", attr "d" "M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z", attr "clip-rule" "evenodd" ] "")
                                )
                                <> div [ attr "class" "ml-3" ]
                                  (tag "p" [ attr "class" "text-sm text-red-800" ] msg)
                            )
                        )
                      Nothing -> ""
                  )
                <> div [ attr "class" "mb-6 flex justify-between items-center" ]
                  ( tag "a" [ attr "href" "/", attr "class" "text-blue-600 hover:text-blue-800" ] "← Back to Schedule" <>
                      tag "a" [ attr "href" "/admin/guest-stays/new", attr "class" "bg-green-600 hover:bg-green-700 text-white px-4 py-2 rounded-lg font-medium" ] "+ Add New Guest Stay"
                  )
                <>
                  div [ attr "class" "bg-white rounded-lg shadow-sm border border-gray-200 overflow-hidden" ]
                    ( if Array.null guestStays then
                        div [ attr "class" "p-8 text-center text-gray-500" ] "No Internal Guest Stays found. Add your first one!"
                      else
                        table [ attr "class" "w-full" ]
                          ( tag "thead" [ attr "class" "bg-gray-50" ]
                              ( tr []
                                  ( th [ attr "class" "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] "Apartment"
                                      <> th [ attr "class" "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] "Dates"
                                      <> th [ attr "class" "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] "Code"
                                      <> th [ attr "class" "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] "Link"
                                      <>
                                        th [ attr "class" "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ] "Actions"
                                  )
                              ) <>
                              tag "tbody" [ attr "class" "divide-y divide-gray-200" ]
                                (Array.foldMap renderGuestStayRow guestStays)
                          )
                    )
            )
        )
  where
  renderGuestStayRow :: GuestStay -> HtmlString
  renderGuestStayRow { id, apartment, fromDate, toDate, last4Digits, link } =
    let
      apartmentName = case apartment of
        Apartment name -> name
      fromDateStr = show (fromEnum $ Date.year fromDate) <> "-"
        <> (if fromEnum (Date.month fromDate) < 10 then "0" else "")
        <> show (fromEnum $ Date.month fromDate)
        <> "-"
        <> (if fromEnum (Date.day fromDate) < 10 then "0" else "")
        <> show (fromEnum $ Date.day fromDate)
      toDateStr = show (fromEnum $ Date.year toDate) <> "-"
        <> (if fromEnum (Date.month toDate) < 10 then "0" else "")
        <> show (fromEnum $ Date.month toDate)
        <> "-"
        <> (if fromEnum (Date.day toDate) < 10 then "0" else "")
        <> show (fromEnum $ Date.day toDate)
    in
      tr [ attr "class" "hover:bg-gray-50" ]
        ( td [ attr "class" "px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900" ] apartmentName
            <> td [ attr "class" "px-6 py-4 whitespace-nowrap text-sm text-gray-900" ] (fromDateStr <> " → " <> toDateStr)
            <> td [ attr "class" "px-6 py-4 whitespace-nowrap text-sm text-gray-900 font-mono" ] last4Digits
            <> td [ attr "class" "px-6 py-4 whitespace-nowrap text-sm text-blue-600" ]
              (tag "a" [ attr "href" link, attr "target" "_blank", attr "class" "hover:underline" ] "View Link")
            <>
              td [ attr "class" "px-6 py-4 whitespace-nowrap text-sm text-gray-500" ]
                ( tag "a" [ attr "href" ("/admin/guest-stays/" <> id <> "/edit"), attr "class" "text-blue-600 hover:text-blue-800 mr-3" ] "Edit" <>
                    tag "a"
                      [ attr "href" ("/admin/guest-stays/" <> id)
                      , attr "class" "text-red-600 hover:text-red-800"
                      , attr "data-turbo-method" "delete"
                      , attr "data-turbo-confirm" "Are you sure you want to delete this guest stay? This action cannot be undone."
                      ]
                      "Delete"
                )
        )

newGuestStayFormPage :: HtmlString
newGuestStayFormPage =
  html $
    head "Admin - Add New Guest Stay" <>
      body
        ( div [ attr "class" "max-w-2xl mx-auto p-6" ]
            ( h1 [ attr "class" "text-3xl font-bold text-center text-gray-800 mb-8" ] "Add New Guest Stay"
                <> div [ attr "class" "mb-6" ]
                  (tag "a" [ attr "href" "/admin/guest-stays", attr "class" "text-blue-600 hover:text-blue-800" ] "← Back to Guest Stays")
                <>
                  div [ attr "class" "bg-white rounded-lg shadow-sm border border-gray-200 p-6" ]
                    ( tag "form" [ attr "method" "POST", attr "action" "/admin/guest-stays" ]
                        ( div [ attr "class" "space-y-4" ]
                            ( div []
                                ( tag "label" [ attr "for" "apartment", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Apartment" <>
                                    tag "select" [ attr "id" "apartment", attr "name" "apartment", attr "required" "true", attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500" ]
                                      ( tag "option" [ attr "value" "" ] "Select an apartment"
                                          <> tag "option" [ attr "value" "Glória" ] "Glória"
                                          <>
                                            tag "option" [ attr "value" "Santa" ] "Santa"
                                      )
                                )
                                <> div []
                                  ( tag "label" [ attr "for" "fromDate", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Check-in Date" <>
                                      tag "input" [ attr "type" "date", attr "id" "fromDate", attr "name" "fromDate", attr "required" "true", attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500" ] ""
                                  )
                                <> div []
                                  ( tag "label" [ attr "for" "toDate", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Check-out Date" <>
                                      tag "input" [ attr "type" "date", attr "id" "toDate", attr "name" "toDate", attr "required" "true", attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500" ] ""
                                  )
                                <> div []
                                  ( tag "label" [ attr "for" "last4Digits", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Last 4 Digits" <>
                                      tag "input" [ attr "type" "text", attr "id" "last4Digits", attr "name" "last4Digits", attr "maxlength" "4", attr "pattern" "[0-9]{4}", attr "required" "true", attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500", attr "placeholder" "1234" ] ""
                                  )
                                <> div []
                                  ( tag "label" [ attr "for" "link", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Booking Reference/Link" <>
                                      tag "input" [ attr "type" "url", attr "id" "link", attr "name" "link", attr "required" "true", attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500", attr "placeholder" "https://..." ] ""
                                  )
                                <>
                                  div [ attr "class" "pt-4" ]
                                    ( tag "button" [ attr "type" "submit", attr "class" "w-full bg-blue-600 hover:bg-blue-700 text-white font-medium py-2 px-4 rounded-lg transition-colors" ] "Add Guest Stay" <>
                                        tag "a" [ attr "href" "/admin/guest-stays", attr "class" "block w-full text-center bg-gray-300 hover:bg-gray-400 text-gray-700 font-medium py-2 px-4 rounded-lg mt-2 transition-colors" ] "Cancel"
                                    )
                            )
                        )
                    )
            )
        )

editGuestStayFormPage :: GuestStay -> HtmlString
editGuestStayFormPage guestStay =
  let
    apartmentName = case guestStay.apartment of
      Apartment name -> name
    fromDateStr = show (fromEnum $ Date.year guestStay.fromDate) <> "-"
      <> (if fromEnum (Date.month guestStay.fromDate) < 10 then "0" else "")
      <> show (fromEnum $ Date.month guestStay.fromDate)
      <> "-"
      <> (if fromEnum (Date.day guestStay.fromDate) < 10 then "0" else "")
      <> show (fromEnum $ Date.day guestStay.fromDate)
    toDateStr = show (fromEnum $ Date.year guestStay.toDate) <> "-"
      <> (if fromEnum (Date.month guestStay.toDate) < 10 then "0" else "")
      <> show (fromEnum $ Date.month guestStay.toDate)
      <> "-"
      <> (if fromEnum (Date.day guestStay.toDate) < 10 then "0" else "")
      <> show (fromEnum $ Date.day guestStay.toDate)
  in
    html $
      head "Admin - Edit Guest Stay" <>
        body
          ( div [ attr "class" "max-w-2xl mx-auto p-6" ]
              ( h1 [ attr "class" "text-3xl font-bold text-center text-gray-800 mb-8" ] "Edit Guest Stay"
                  <> div [ attr "class" "mb-6" ]
                    (tag "a" [ attr "href" "/admin/guest-stays", attr "class" "text-blue-600 hover:text-blue-800" ] "← Back to Guest Stays")
                  <>
                    div [ attr "class" "bg-white rounded-lg shadow-sm border border-gray-200 p-6" ]
                      ( tag "form" [ attr "method" "POST", attr "action" ("/admin/guest-stays/" <> guestStay.id), attr "onsubmit" "this.querySelector('input[name=\"_method\"]').value='PUT'" ]
                          ( tag "input" [ attr "type" "hidden", attr "name" "_method", attr "value" "PUT" ] "" <>
                              div [ attr "class" "space-y-4" ]
                                ( div []
                                    ( tag "label" [ attr "for" "apartment", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Apartment" <>
                                        tag "select" [ attr "id" "apartment", attr "name" "apartment", attr "required" "true", attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500" ]
                                          ( tag "option" [ attr "value" "Glória", if apartmentName == "Glória" then attr "selected" "true" else attr "class" "" ] "Glória"
                                              <>
                                                tag "option" [ attr "value" "Santa", if apartmentName == "Santa" then attr "selected" "true" else attr "class" "" ] "Santa"
                                          )
                                    )
                                    <> div []
                                      ( tag "label" [ attr "for" "fromDate", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Check-in Date" <>
                                          tag "input" [ attr "type" "date", attr "id" "fromDate", attr "name" "fromDate", attr "required" "true", attr "value" fromDateStr, attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500" ] ""
                                      )
                                    <> div []
                                      ( tag "label" [ attr "for" "toDate", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Check-out Date" <>
                                          tag "input" [ attr "type" "date", attr "id" "toDate", attr "name" "toDate", attr "required" "true", attr "value" toDateStr, attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500" ] ""
                                      )
                                    <> div []
                                      ( tag "label" [ attr "for" "last4Digits", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Last 4 Digits" <>
                                          tag "input" [ attr "type" "text", attr "id" "last4Digits", attr "name" "last4Digits", attr "maxlength" "4", attr "pattern" "[0-9]{4}", attr "required" "true", attr "value" guestStay.last4Digits, attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500", attr "placeholder" "1234" ] ""
                                      )
                                    <> div []
                                      ( tag "label" [ attr "for" "link", attr "class" "block text-sm font-medium text-gray-700 mb-1" ] "Booking Reference/Link" <>
                                          tag "input" [ attr "type" "url", attr "id" "link", attr "name" "link", attr "required" "true", attr "value" guestStay.link, attr "class" "block w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500", attr "placeholder" "https://..." ] ""
                                      )
                                    <>
                                      div [ attr "class" "pt-4" ]
                                        ( tag "button" [ attr "type" "submit", attr "class" "w-full bg-blue-600 hover:bg-blue-700 text-white font-medium py-2 px-4 rounded-lg transition-colors" ] "Update Guest Stay" <>
                                            tag "a" [ attr "href" "/admin/guest-stays", attr "class" "block w-full text-center bg-gray-300 hover:bg-gray-400 text-gray-700 font-medium py-2 px-4 rounded-lg mt-2 transition-colors" ] "Cancel"
                                        )
                                )
                          )
                      )
              )
          )