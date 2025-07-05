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
  , months :: Array String
  , weekdays :: Array String
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
  , months: 
    [ "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho"
    , "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
    ]
  , weekdays:
    [ "Domingo", "Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado"
    ]
  }

formatDatePt :: DateTime -> String
formatDatePt dt =
  let
    date = DateTime.date dt
    day = fromEnum $ DateTime.day date
    monthIndex = (fromEnum $ DateTime.month date) - 1
    year = fromEnum $ DateTime.year date
    weekdayIndex = case DateTime.weekday date of
      Sunday -> 0
      Monday -> 1
      Tuesday -> 2
      Wednesday -> 3
      Thursday -> 4
      Friday -> 5
      Saturday -> 6
    
    monthName = case Array.index pt.months monthIndex of
      Just name -> name
      Nothing -> show (monthIndex + 1)
    
    weekdayName = case Array.index pt.weekdays weekdayIndex of
      Just name -> name
      Nothing -> show weekdayIndex
  in
    show day <> " de " <> monthName <> " de " <> show year <> " (" <> weekdayName <> ")"