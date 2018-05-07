module Main where

import Prelude

import Control.Monad.Aff (Error, runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Database.RethinkDB (ConnectionConfig, DB, Table(..), close, connect, getAll)

newtype User = User
  { id :: String
  , email :: String
  , firstName :: String
  , lastName :: String
  }

derive instance userGeneric :: Generic User _

instance userDocode :: Decode User where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance userShow :: Show User where
  show = genericShow

main :: forall e. Eff (console :: CONSOLE, db :: DB | e) Unit
main = runAff_ logConnection do
  connection <- connect connectionConfig
  user :: Array User <- getAll ["40867372-d628-4b81-8a80-0b947c46d279"] (Table "user") connection
  liftEff $ log $ show user
  close connection
    where
      logConnection :: forall a eff. Either Error a -> Eff (console :: CONSOLE | eff) Unit
      logConnection (Left err) = log $ show err
      logConnection (Right _) = log "Connected!"

connectionConfig :: ConnectionConfig
connectionConfig = {
  host: "localhost",
  port: 28015,
  db: "tomorrow",
  user: "admin",
  password: "",
  timeout: 20
}
