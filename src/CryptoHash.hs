module CryptoHash where

import qualified Crypto.KDF.BCrypt as BCrypt
import qualified Crypto.Random     as CR
import           Polysemy          (Member, Sem)
import qualified Polysemy          as P
import qualified Polysemy.Internal as P
import           Polysemy.State    (State)
import qualified Polysemy.State    as PS
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


-- | Interpret CryptoHash using the BCrypt algorithm. This requires a
-- DRG in a State effect.
runCryptoHashAsState :: (CR.DRG gen, Member (State gen) r) => Sem (CryptoHash : r) a -> Sem r a
runCryptoHashAsState = P.interpret $ \case
  ValidateHash password hash -> return $ BCrypt.validatePassword password hash
  MakeHash password          -> do
    drg <- PS.get
    let (hash, drg') = CR.withDRG drg $ BCrypt.hashPassword 5 password
    PS.put drg'
    return hash
