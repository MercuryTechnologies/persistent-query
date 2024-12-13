{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.Query.Types
  ( Error (..),
    QueryError (..),
    toQueryError,
  )
where

import Control.Exception
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Coerce
import Database.Esqueleto.Experimental
import Data.UUID
import Data.Text
import Prelude

data Error where
  NoRows :: Error
  MoreThanOneRow :: Error
  EntityNotFound :: (PersistEntity t, Coercible UUID (Key t)) => Key t -> Error
  MoreThanOneEntity :: (PersistEntity t, Coercible UUID (Key t)) => Key t -> Error
  Other :: Text -> Error

data QueryError = QueryError
  { reason :: Text
  , key :: Maybe UUID
  }
  deriving (Eq, Show)

instance Exception QueryError

$(deriveJSON defaultOptions ''QueryError)

toQueryError :: Error -> QueryError
toQueryError NoRows = QueryError "NoRows" Nothing
toQueryError MoreThanOneRow = QueryError "MoreThanOneRow" Nothing
toQueryError (EntityNotFound key) = keyToQueryError "EntityNotFound" key
toQueryError (MoreThanOneEntity key) = keyToQueryError "MoreThanOneEntity" key
toQueryError (Other txt) = QueryError txt Nothing

keyToQueryError :: (PersistEntity t, Coercible UUID (Key t)) => Text -> Key t -> QueryError
keyToQueryError reason key =
  let def = entityDef key
      name = unEntityNameHS $ getEntityHaskellName def
   in QueryError (reason <> " " <> name) (Just $ coerce key)
