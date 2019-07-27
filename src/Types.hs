module Types where

import Data.ByteString (ByteString)


-- A few type definitions to get started
newtype Username = Username ByteString

newtype Password = Password ByteString

newtype PasswordHash = PasswordHash ByteString
