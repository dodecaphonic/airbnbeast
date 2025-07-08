module Airbnbeast.Session where

import Prelude
import Airbnbeast.Auth (User, SessionId(..), Session, UserId(..), Username(..))
import Node.Crypto (randomSessionToken, hmacSign, verifyHmac)
import Data.String as String
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Data.Array as Array
import Data.Int as Int
import Effect.Now (nowDateTime)
import Data.Time.Duration (Hours(..))

-- | Session token configuration
type SessionConfig =
  { secret :: String
  , maxAge :: Hours -- Session expiry time
  }

-- | Default session configuration (12 hours)
defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  { secret: "your-secret-key-change-this-in-production"
  , maxAge: Hours 12.0
  }

-- | Create a new secure session
createSession :: SessionConfig -> User -> Aff String
createSession config user = do
  sessionId <- liftEffect $ randomSessionToken 32
  _ <- liftEffect nowDateTime
  let
    Hours maxAgeHours = config.maxAge
    -- Calculate expiry time (current time + maxAge hours)
    expiryMillis = show $ Int.round $ maxAgeHours * 60.0 * 60.0 * 1000.0

    sessionData =
      sessionId <> "|"
        <> (case user.id of UserId id -> show id)
        <> "|"
        <> (case user.username of Username name -> name)
        <> "|"
        <> show user.isAdmin
        <> "|"
        <> expiryMillis

    signature = hmacSign config.secret sessionData
    signedSession = sessionData <> "|" <> signature

  pure signedSession

-- | Validate and parse a session cookie
validateSession :: SessionConfig -> String -> Maybe Session
validateSession config cookieValue = do
  let parts = String.split (String.Pattern "|") cookieValue
  case parts of
    [ sessionId, userIdStr, usernameStr, isAdminStr, expiryStr, signature ] -> do
      -- Reconstruct the data to verify signature
      let sessionData = sessionId <> "|" <> userIdStr <> "|" <> usernameStr <> "|" <> isAdminStr <> "|" <> expiryStr

      -- Verify HMAC signature
      if verifyHmac config.secret sessionData signature then do
        -- Parse user data
        userId <- Int.fromString userIdStr
        isAdmin <- case isAdminStr of
          "true" -> Just true
          "false" -> Just false
          _ -> Nothing

        pure
          { sessionId: SessionId sessionId
          , user:
              { id: UserId userId
              , username: Username usernameStr
              , isAdmin: isAdmin
              }
          }
      else Nothing
    _ -> Nothing

-- | Create a secure cookie header
createSecureCookie :: String -> String -> String
createSecureCookie name value =
  name <> "=" <> value <> "; Path=/; HttpOnly; Secure; SameSite=Strict"

-- | Create a session cookie header
createSessionCookie :: String -> String
createSessionCookie sessionToken =
  createSecureCookie "_airbnbeast_session" sessionToken

-- | Parse cookie header to extract session
parseSessionFromCookie :: String -> Maybe String
parseSessionFromCookie cookieHeader = do
  let cookies = String.split (String.Pattern ";") cookieHeader
  sessionCookie <- Array.find (String.contains (String.Pattern "_airbnbeast_session=")) cookies
  String.stripPrefix (String.Pattern "_airbnbeast_session=") $ String.trim sessionCookie