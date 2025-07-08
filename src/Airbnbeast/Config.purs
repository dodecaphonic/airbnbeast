module Airbnbeast.Config where

import Airbnbeast.Session (SessionConfig)
import Airbnbeast.Storage (Storage)

type AirbnbeastConfig =
  { storage :: Storage
  , sessionConfig :: SessionConfig
  }