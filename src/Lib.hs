module Lib where

import           CryptoHash
import           Polysemy         (Members, Sem)
import           Polysemy.KVStore (KVStore (..))
import qualified Polysemy.KVStore as Store
import           Types


--
-- The operations to be implemented by the password manager
--
addUser :: Members [CryptoHash, KVStore Username PasswordHash] r
        => Username
        -> Password
        -> Sem r ()
addUser username password = do
    hashedPassword <- makeHash password
    Store.writeKV username hashedPassword

validatePassword :: Members [CryptoHash, KVStore Username PasswordHash] r
                 => Username
                 -> Password
                 -> Sem r Bool
validatePassword username password = do
    hashInStore <- Store.lookupKV username
    case hashInStore of
      Just h  -> validateHash password h
      Nothing -> return False
