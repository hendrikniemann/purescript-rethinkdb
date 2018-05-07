module Database.RethinkDB
  ( Connection
  , Table(..)
  , Index(..)
  , ConnectionConfig
  , connect
  , close
  , DB
  , get
  , getAll
  , getAllFromIndex
  ) where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (class Decode, decode)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)

foreign import data Connection :: Type

foreign import data DB :: Effect

type Key = String

newtype Table = Table String

newtype Index = Index String

type ConnectionConfig =
  { host :: String
  , port :: Int
  , db :: String
  , user :: String
  , password :: String
  , timeout :: Int
  -- TODO: ssl
  }

connect :: forall eff. ConnectionConfig -> Aff (db :: DB | eff) Connection
connect = fromEffFnAff <<< _connect

close :: forall eff. Connection -> Aff (db :: DB | eff) Unit
close = fromEffFnAff <<< _close

get :: forall eff a
  . Decode a
  => Key -> Table -> Connection -> Aff (db :: DB | eff) (Maybe a)
get key table connection = do
  res <- fromEffFnAff $ _get key table connection
  pure $ either (const Nothing) Just (runExcept (decode res))

-- | Select all values with the given primary index keys from a table.
getAll :: forall eff a
  . Decode a
  => Array Key -> Table -> Connection -> Aff (db :: DB | eff) (Array a) 
getAll keys table connection = do
  results <- fromEffFnAff $ _getAll keys table connection
  either liftError pure (runExcept (sequence $ map decode results))

-- | Select all values with the specified index' keys from a table.
getAllFromIndex :: forall eff a
  . Decode a
  => Array Key -> Table -> Index -> Connection -> Aff (db :: DB | eff) (Array a)
getAllFromIndex keys table index connection = do
  results <- fromEffFnAff $ _getAllFromIndex keys table index connection
  either liftError pure (runExcept (sequence $ map decode results))

foreign import _connect :: forall eff
  . ConnectionConfig -> EffFnAff (db :: DB | eff) Connection

foreign import _close :: forall eff
  . Connection -> EffFnAff (db :: DB | eff) Unit

foreign import _get :: forall eff
  . Key -> Table -> Connection -> EffFnAff (db :: DB | eff) Foreign

foreign import _getAll :: forall eff
  . Array Key -> Table -> Connection -> EffFnAff (db :: DB | eff) (Array Foreign)

foreign import _getAllFromIndex :: forall eff
  . Array Key
  -> Table
  -> Index
  -> Connection
  -> EffFnAff (db :: DB | eff) (Array Foreign)

liftError :: forall e a. MultipleErrors -> Aff e a
liftError errs = throwError $ error (show errs)
