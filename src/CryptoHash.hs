module CryptoHash where

import           Polysemy          (Member, Sem)
import qualified Polysemy.Internal as P
import           Types


-- | An effect for cryptographic hashing of passwords. This is defined
-- as a GADT whose constructors represent the actions supported by
-- this effect.
--
-- The type parameter m represents an arbitrary monad and the type
-- parameter a is the type of values returned by the actions.
data CryptoHash m a where
  -- | Generates a hash from a password
  MakeHash :: Password -> CryptoHash m PasswordHash
  -- | Check if a password matches a hash
  ValidateHash :: Password -> PasswordHash -> CryptoHash m Bool

-- | CryptoHash actions
--
-- These are functions that correspond to the constructors of
-- @CryptoHash@ and let you embed an effect in the @Sem@ monad.
--
-- Writing these functions by hand is tedious. You can generate them
-- with the following TemplateHaskell code instead if you choose to do
-- so.
--
-- P.makeSem ''CryptoHash
--
makeHash :: Member CryptoHash r => Password -> Sem r PasswordHash
makeHash x = P.send (MakeHash x :: CryptoHash (Sem r) PasswordHash)

validateHash :: Member CryptoHash r => Password -> PasswordHash -> Sem r Bool
validateHash password hash = P.send (ValidateHash password hash :: CryptoHash (Sem r) Bool)
