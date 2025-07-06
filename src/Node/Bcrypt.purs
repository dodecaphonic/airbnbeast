module Node.Bcrypt where

import Prelude

foreign import hashPasswordImpl :: String -> String
foreign import comparePasswordImpl :: String -> String -> Boolean

hashPassword :: String -> String
hashPassword = hashPasswordImpl

comparePassword :: String -> String -> Boolean
comparePassword password hash = comparePasswordImpl password hash