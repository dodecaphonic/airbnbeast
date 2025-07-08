module Node.Crypto where

import Prelude
import Effect (Effect)
import Data.Function.Uncurried (Fn2, runFn2)

-- | Generate HMAC signature for data using a secret key
foreign import hmacImpl :: Fn2 String String String

-- | Generate a secure random session token
foreign import randomBytesImpl :: Int -> Effect String

-- | Generate a secure random session token
randomSessionToken :: Int -> Effect String
randomSessionToken = randomBytesImpl

-- | Sign data using HMAC-SHA256
hmacSign :: String -> String -> String
hmacSign secret dataString = runFn2 hmacImpl secret dataString

-- | Verify HMAC signature
verifyHmac :: String -> String -> String -> Boolean
verifyHmac secret dataString signature =
  let
    expectedSignature = hmacSign secret dataString
  in
    signature == expectedSignature