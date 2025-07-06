module Airbnbeast.Auth where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- User data types
newtype UserId = UserId Int
newtype Username = Username String

derive instance Generic UserId _
derive instance Generic Username _
derive instance Eq UserId
derive instance Eq Username

instance Show UserId where
  show = genericShow

instance Show Username where
  show = genericShow

type User =
  { id :: UserId
  , username :: Username
  , isAdmin :: Boolean
  }

-- Session data
newtype SessionId = SessionId String

derive instance Generic SessionId _
derive instance Eq SessionId

instance Show SessionId where
  show = genericShow

type Session =
  { sessionId :: SessionId
  , user :: User
  }

-- Authentication errors
data AuthError
  = InvalidCredentials
  | UserNotFound
  | SessionNotFound
  | DatabaseError String

derive instance Generic AuthError _
derive instance Eq AuthError

instance Show AuthError where
  show = genericShow