module Airbnbeast.I18n where

import Prelude

import Data.Array as Array
import Data.DateTime (DateTime, Weekday(..))
import Data.DateTime as DateTime
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))

type Translations =
  { pageTitle :: String
  , cleaningSchedule :: String
  , apartmentSchedule :: String -> String
  , cleaningWindow :: String
  , keypadCode :: String
  , weekend :: String
  , reservation :: String
  , viewReservation :: String
  , noCleaningWindows :: String
  , fullWeekend :: String
  , partialWeekend :: String
  , weekdayOnly :: String
  , weekdays :: Array String
  , adjustPeriods :: String
  , hidePeriods :: String
  , clickToToggle :: String
  , morning :: String
  , afternoon :: String
  }

pt :: Translations
pt =
  { pageTitle: "Cronograma de Limpeza - Airbnbeast"
  , cleaningSchedule: "🧹 Cronograma de Limpeza"
  , apartmentSchedule: \name -> "🧹 Cronograma de Limpeza - " <> name
  , cleaningWindow: "Período de Limpeza"
  , keypadCode: "Código do Portão"
  , weekend: "Fim de Semana"
  , reservation: "Reserva"
  , viewReservation: "Ver Reserva"
  , noCleaningWindows: "Nenhum período de limpeza agendado"
  , fullWeekend: "Fim de Semana Completo"
  , partialWeekend: "Fim de Semana Parcial"
  , weekdayOnly: "Apenas Dias Úteis"
  , weekdays:
      [ "Dom"
      , "Seg"
      , "Ter"
      , "Qua"
      , "Qui"
      , "Sex"
      , "Sáb"
      ]
  , adjustPeriods: "⚙️ Ajustar Períodos"
  , hidePeriods: "⚙️ Ocultar Períodos"
  , clickToToggle: "Clique para bloquear/desbloquear períodos:"
  , morning: "Manhã"
  , afternoon: "Tarde"
  }

formatDatePt :: DateTime -> String
formatDatePt dt =
  let
    date = DateTime.date dt
    day = fromEnum $ DateTime.day date
    month = fromEnum $ DateTime.month date
    year = fromEnum $ DateTime.year date
    weekdayIndex = case DateTime.weekday date of
      Sunday -> 0
      Monday -> 1
      Tuesday -> 2
      Wednesday -> 3
      Thursday -> 4
      Friday -> 5
      Saturday -> 6

    dayStr = if day < 10 then "0" <> show day else show day
    monthStr = if month < 10 then "0" <> show month else show month

    weekdayName = case Array.index pt.weekdays weekdayIndex of
      Just name -> name
      Nothing -> show weekdayIndex
  in
    "<span class=\"font-semibold\">" <> dayStr <> "/" <> monthStr <> "/" <> show year <> "</span> <span class=\"font-normal\">(" <> weekdayName <> ")</span>"