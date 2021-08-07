module CryptoHash where

import qualified Polysemy          as P

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

P.makeSem ''CryptoHash
