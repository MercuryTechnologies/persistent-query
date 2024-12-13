module Database.Persist.Query.WithError
  ( module Database.Persist.Query.Common,
    single,
    atLeastOne,
    firstOfMany,
    atLeastOne_,
    oneOrNone,
    getRecord,
    Error (..),
    QueryError (..),
    toQueryError,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Database.Persist.Query.Common
import Database.Persist.Query.Types
import Database.Esqueleto.Experimental qualified as E
import GHC.Stack
import Prelude

single :: HasCallStack => e -> e -> [r] -> Query e r
single noRowsError moreThanOneRowError lst = case lst of
  [] -> unexpectedAt callStack noRowsError
  (_ : _ : _) -> unexpectedAt callStack moreThanOneRowError
  [x] -> pure x

atLeastOne :: HasCallStack => e -> [r] -> Query e (NonEmpty r)
atLeastOne noRowsError lst = case lst of
  [] -> unexpectedAt callStack noRowsError
  (x : xs) -> pure (x NE.:| xs)

-- | Like 'atLeastOne' but only the first one is given.
firstOfMany :: HasCallStack => e -> [r] -> Query e r
firstOfMany noRowsError lst = case lst of
  [] -> unexpectedAt callStack noRowsError
  (x : _) -> pure x

atLeastOne_ :: HasCallStack => e -> [r] -> Query e r
atLeastOne_ = firstOfMany

-- | Zero or one row, returning an error if there are more than one.
oneOrNone :: HasCallStack => e -> [r] -> Query e (Maybe r)
oneOrNone moreThanOneRowError lst = case lst of
  [] -> pure Nothing
  [r] -> pure (Just r)
  _ : _ : _ -> unexpectedAt callStack moreThanOneRowError

-- | Get a single Entity, with @Error@
getRecord ::
  forall e record.
  (HasCallStack, E.PersistEntity record) =>
  e ->
  e ->
  E.Key record ->
  Query e record
getRecord noRowsError moreThanOneRowError theId =
  fmap E.entityVal
    -- Make sure the getRecord call is the end of the call stack (single
    -- uses HasCallStack too)
    -- Could also use withFrozenCallStack to the same end.
    <$> exceptionAt (const [callStack])
    $ single noRowsError moreThanOneRowError
      =<< ( query $ do
              t <- E.from $ E.table @record
              E.where_ $ t E.^. E.persistIdField E.==. E.val theId
              pure t
          )
