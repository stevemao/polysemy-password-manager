module Main where

import qualified Crypto.Random          as CR
import           CryptoHash
import           Data.Function          ((&))
import           Data.Maybe             (listToMaybe)
import           Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as SQL
import           Lib
import           Polysemy               (Embed, Member, Members, Sem)
import qualified Polysemy               as P
import qualified Polysemy.Input         as PI
import           Polysemy.KVStore       (KVStore (..))
import qualified Polysemy.State         as PS
import           Types
import qualified Crypto.KDF.BCrypt as BCrypt
import           Polysemy.State    (State)

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

-- | Run a KVStore effect on an SQLite backend. Requires a DB connection as input.
runKVStoreAsSQLite :: Member (Embed IO) r
                   => Sem (KVStore Username PasswordHash : r) a
                   -> Sem (PI.Input SQL.Connection : r) a
runKVStoreAsSQLite = P.reinterpret $ \case
  LookupKV username -> do
    conn <- PI.input
    hashes <- P.embed $ SQL.queryNamed conn
              "SELECT hash FROM passwords WHERE username = :username"
              [":username" := username]
    return (SQL.fromOnly <$> listToMaybe hashes)
  UpdateKV username maybeHash -> do
    let (query, params) =
          case maybeHash of
            Just hash -> ( "INSERT INTO passwords (username, hash) VALUES (:username, :hash) " <>
                           "ON CONFLICT (username) DO UPDATE SET hash = excluded.hash"
                         , [":username" := username, ":hash" := hash] )
            Nothing   -> ( "DELETE FROM passwords WHERE username = :username"
                         , [":username" := username] )
    conn <- PI.input
    P.embed $ SQL.executeNamed conn query params


-- | Takes a program with effects and handles each effect till it gets
-- reduced to IO a.
--
-- The comments on the rightside of each line below indicates the list
-- of effects that still need to be handled at that point.
--
runAllEffects :: CR.DRG gen
              => gen
              -> SQL.Connection
              -> (forall r. Members [CryptoHash, KVStore Username PasswordHash] r => Sem r a)
              -> IO a
runAllEffects drg conn program =
  program                    -- [CryptoHash, KVStore Username PasswordHash]
    & runCryptoHashAsState   -- [KVStore Username PasswordHash, State gen]
    & runKVStoreAsSQLite     -- [Input Connection, State gen, Embed IO]
    & PI.runInputConst conn  -- [State gen, Embed IO]
    & PS.evalState drg       -- [Embed IO]
    & P.runM

--
-- Interpret the operations
--

dbFile :: FilePath
dbFile = "password-manager.db"

withPasswordDBConnection :: (SQL.Connection -> IO a) -> IO a
withPasswordDBConnection f = SQL.withConnection dbFile $ \conn -> do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS passwords (username TEXT PRIMARY KEY, hash TEXT)"
  f conn

runAddUser :: SQL.Connection -> Username -> Password -> IO ()
runAddUser conn username password = do
  drg <- CR.getSystemDRG
  runAllEffects drg conn (addUser username password)

runValidatePassword :: SQL.Connection -> Username -> Password -> IO Bool
runValidatePassword conn username password = do
  drg <- CR.getSystemDRG
  runAllEffects drg conn (validatePassword username password)


main :: IO ()
main =
  withPasswordDBConnection $ \conn -> do
    putStrLn "Adding a username and password to the store"
    runAddUser conn "avengers" "assemble"

    putStr "Validating a good password: "
    runValidatePassword conn "avengers" "assemble" >>= printResult

    putStr "Validating a bad password: "
    runValidatePassword conn "avengers" "runaway" >>= printResult

  where
    printResult True  = putStrLn "Accepted"
    printResult False = putStrLn "Rejected"
