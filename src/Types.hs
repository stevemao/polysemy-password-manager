module Types where

import Data.ByteArray                   (ByteArray, ByteArrayAccess)
import Data.ByteString                  (ByteString)
import Data.String                      (IsString)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField   (ToField)


-- A few type definitions to get started
newtype Username = Username ByteString
  deriving (Eq, Ord, Show, ToField, IsString)

newtype Password = Password ByteString
  deriving (Eq, Ord, Show, Semigroup, Monoid, ByteArrayAccess, ByteArray, IsString)

newtype PasswordHash = PasswordHash ByteString
  deriving (Eq, Ord, Show, Semigroup, Monoid, ByteArrayAccess, ByteArray, ToField, FromField)
