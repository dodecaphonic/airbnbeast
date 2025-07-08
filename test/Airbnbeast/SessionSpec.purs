module Test.Airbnbeast.SessionSpec where

import Prelude

import Airbnbeast.Session (SessionConfig, createSession, validateSession, parseSessionFromCookie, createSessionCookie, defaultSessionConfig)
import Airbnbeast.Auth (UserId(..), Username(..), User, Session)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import Effect.Console (log)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy, fail)

sessionSpecs :: forall m g. Monad m => MonadThrow Error g => MonadEffect g => MonadAff g => SpecT g Unit m Unit
sessionSpecs = do
  describe "Airbnbeast.Session" do
    let
      testUser :: User
      testUser = 
        { id: UserId 123
        , username: Username "testuser"
        , isAdmin: false
        }
      
      testConfig :: SessionConfig
      testConfig = 
        { secret: "test-secret-key-for-hmac-signing"
        , maxAge: Hours 1.0
        }

    describe "createSession" do
      it "creates a session token with user data" do
        sessionToken <- liftAff $ createSession testConfig testUser
        
        -- Token should be non-empty
        sessionToken `shouldSatisfy` (_ /= "")
        
        -- Token should contain pipe-separated data
        sessionToken `shouldSatisfy` (String.contains (String.Pattern "|"))

      it "creates different tokens for different users" do
        let 
          otherUser = 
            { id: UserId 456
            , username: Username "otheruser"
            , isAdmin: true
            }
        
        token1 <- liftAff $ createSession testConfig testUser
        token2 <- liftAff $ createSession testConfig otherUser
        
        token1 `shouldNotEqual` token2

    describe "validateSession" do
      it "validates a valid session token" do
        sessionToken <- liftAff $ createSession testConfig testUser
        let maybeSession = validateSession testConfig sessionToken
        
        case maybeSession of
          Just session -> do
            session.user.id `shouldEqual` testUser.id
            session.user.username `shouldEqual` testUser.username
            session.user.isAdmin `shouldEqual` testUser.isAdmin
          Nothing ->
            fail "Expected session to be valid"

      it "rejects an invalid session token" do
        let invalidToken = "invalid|token|data|signature"
        let maybeSession = validateSession testConfig invalidToken
        
        maybeSession `shouldEqual` Nothing

      it "rejects a tampered session token" do
        sessionToken <- liftAff $ createSession testConfig testUser
        -- Tamper with the token by changing a character
        let tamperedToken = String.replace (String.Pattern "a") (String.Replacement "b") sessionToken
        let maybeSession = validateSession testConfig tamperedToken
        
        maybeSession `shouldEqual` Nothing

    describe "parseSessionFromCookie" do
      it "extracts session token from cookie header" do
        let 
          sessionToken = "abc123def456"
          cookieHeader = "_airbnbeast_session=" <> sessionToken <> "; other=value"
        
        parseSessionFromCookie cookieHeader `shouldEqual` Just sessionToken

      it "handles cookie header with only session cookie" do
        let 
          sessionToken = "abc123def456"
          cookieHeader = "_airbnbeast_session=" <> sessionToken
        
        parseSessionFromCookie cookieHeader `shouldEqual` Just sessionToken

      it "returns Nothing when no session cookie present" do
        let cookieHeader = "other=value; another=data"
        
        parseSessionFromCookie cookieHeader `shouldEqual` Nothing

    describe "createSessionCookie" do
      it "creates a properly formatted secure cookie" do
        let sessionToken = "abc123def456"
        let cookie = createSessionCookie sessionToken
        
        -- Should contain the session token
        cookie `shouldSatisfy` (String.contains (String.Pattern sessionToken))
        
        -- Should have security flags
        cookie `shouldSatisfy` (String.contains (String.Pattern "HttpOnly"))
        cookie `shouldSatisfy` (String.contains (String.Pattern "Secure"))
        cookie `shouldSatisfy` (String.contains (String.Pattern "SameSite=Strict"))

    describe "integration test" do
      it "full session lifecycle works correctly" do
        -- Create session
        sessionToken <- liftAff $ createSession testConfig testUser
        
        -- Create cookie
        let cookie = createSessionCookie sessionToken
        
        -- Extract token from cookie (simulating browser behavior)
        case parseSessionFromCookie cookie of
          Nothing -> fail "Failed to parse session from cookie"
          Just extractedToken -> do
            -- Validate extracted token
            case validateSession testConfig extractedToken of
              Nothing -> fail "Failed to validate extracted session"
              Just session -> do
                session.user.id `shouldEqual` testUser.id
                session.user.username `shouldEqual` testUser.username
                session.user.isAdmin `shouldEqual` testUser.isAdmin