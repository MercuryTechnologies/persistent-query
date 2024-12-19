module Database.Persist.Query
  ( module Database.Persist.Query.Common,
    single,
    atLeastOne,
    firstOfMany,
    oneOrNone,
    getRecord,
    Error (..),
    QueryError (..),
    toQueryError,
    errorAsText,
    textAsError,
  )
where

import Data.Coerce (Coercible)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Database.Persist.Query.Common
import Database.Persist.Query.Types
import Database.Persist.Query.WithError qualified as QWE
import Database.Esqueleto.Experimental qualified as E
import Data.UUID (UUID)
import GHC.Stack
import Prelude

single :: HasCallStack => [r] -> Query Error r
single = QWE.single NoRows MoreThanOneRow

atLeastOne :: HasCallStack => [r] -> Query Error (NonEmpty r)
atLeastOne = QWE.atLeastOne NoRows

-- | Like 'atLeastOne' but only the first one is given.
firstOfMany :: HasCallStack => [r] -> Query Error r
firstOfMany = QWE.firstOfMany NoRows

-- | Zero or one row, returning an error if there are more than one.
oneOrNone :: HasCallStack => [r] -> Query Error (Maybe r)
oneOrNone = QWE.oneOrNone MoreThanOneRow

-- | Get a single Entity, with @Error@
getRecord ::
  forall record.
  (HasCallStack, E.PersistEntity record, Coercible UUID (E.Key record)) =>
  E.Key record ->
  Query Error record
getRecord theId = QWE.getRecord (EntityNotFound theId) (MoreThanOneEntity theId) theId

errorAsText :: Query Error a -> Query Text a
errorAsText = exceptionAs (reason . toQueryError)

textAsError :: Query Text a -> Query Error a
textAsError = exceptionAs Other
