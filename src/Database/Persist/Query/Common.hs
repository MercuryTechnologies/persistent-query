module Database.Persist.Query.Common
  ( QueryPart (..),
    runQueryPart,
    Query (..),
    queryCheckpoint,
    queryWith,
    query,
    querySingle,
    exists,
    notExists,
    rawSql,
    maybeOne,
    getFirstRecordByField,
    withDefault,
    first,
    unexpected,
    unexpectedAt,
    silent,
    exceptionAs,
    exceptionAt,
    withException,
    withExceptionAndStackTrace,
    E.SqlSelect,
  )
where

import Data.Maybe (fromMaybe)
import Control.Monad (ap)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Reader as Reader (Reader, ask, runReader)
import Data.Text (Text)
import Database.Esqueleto.Internal.Internal as E (SqlSelect)
import Database.Esqueleto.Experimental qualified as E
import Database.Persist qualified as P
import Database.Persist.Sql qualified as P
import GHC.Stack
import Prelude

data QueryPart t where
  -- | An esqueleto selection
  QueryPart :: E.SqlSelect a r => E.SqlQuery a -> ([r] -> t) -> QueryPart t
  -- | An escape hatch to allow for a call to 'Database.Persist.Sql.rawSql',
  -- in case you need to do something which can't be expressed as an esqueleto
  -- query.
  --
  -- NB: you could do non-read-only stuff here, so be careful.
  --
  -- This part contains the arguments to 'Database.Persist.Sql.rawSql', but
  -- they depend on a SqlBackend because that's needed in order to resolve
  -- table and field names.
  RawSqlPart :: (P.RawSql r) => (P.SqlBackend -> (Text, [P.PersistValue])) -> ([r] -> t) -> QueryPart t

instance Functor QueryPart where
  fmap f (QueryPart q k) = QueryPart q (f . k)
  fmap f (RawSqlPart mkSql k) = RawSqlPart mkSql (f . k)

runQueryPart :: QueryPart t -> P.SqlPersistT IO t
runQueryPart (QueryPart q k) = k <$> E.select q
runQueryPart (RawSqlPart mkSql k) = k <$> runRawSql mkSql

-- | Helper function to define 'runQueryPart'.
runRawSql :: P.RawSql t => (P.SqlBackend -> (Text, [P.PersistValue])) -> P.SqlPersistT IO [t]
runRawSql mkSql = do
  sqlBackend <- Reader.ask
  let (sqlText, params) = mkSql sqlBackend
  P.rawSql sqlText params

-- | Whenever you use @queryCheckpoint q@ and there is a @HasCallStack@ in scope,
-- any @unexpected@s in the query @q@ will get the current call site included
-- in the stack trace.
--
-- Best demonstrated by example:
--
-- @
--   topLevelQuery :: HasCallStack => Query Oops Int
--   topLevelQuery = queryCheckpoint $ do
--     x <- subQuery1
--     y <- subQuery2
--     pure (x + y)
-- @
--
-- any @Oops@ coming from @subQuery1@ or @subQuery2@ will include in its
-- stack trace the name @topLevelQuery@ and the location of its caller.
queryCheckpoint :: HasCallStack => Query e t -> Query e t
queryCheckpoint q = Query $ \end run -> unQuery q (pushCaller [callStack] end) run
  where
    pushCaller ::
      [CallStack] ->
      ([CallStack] -> e -> m r) ->
      ([CallStack] -> e -> m r)
    pushCaller thisCStack k = \finalCStack -> k (finalCStack ++ thisCStack)

    -- -- If the caller of @queryCheckpoint@ put @HasCallStack@ then it should be the
    -- -- second frame from the top.
    -- getCaller :: CallStack -> Maybe ([Char], SrcLoc)
    -- getCaller cstack = case getCallStack cstack of
    --   _:caller:_ -> Just caller
    --   _ -> Nothing

    -- maybePushCallStack :: Maybe ([Char], SrcLoc) -> CallStack -> CallStack
    -- maybePushCallStack Nothing = id
    -- maybePushCallStack (Just frame) = pushCallStack frame

-- | A monad in which there are 0 or more SqlQuery parts, and the possibility for
-- early termination.
--
-- Use this if you want to monadically construct read-only database IOs.
--
-- Unlike the DB monad, which tries to make sure you can't do arbitrary IO,
-- this Query monad absolutely makes sure you can't do any IO other than what
-- esqueleto does to run a query, and it also doesn't require that you have
-- a yesod logger.
newtype Query e t = Query
  { unQuery :: forall m.
      Monad m =>
      (forall r. [CallStack] -> e -> m r) ->
      (forall r. QueryPart r -> m r) ->
      m t
  }

instance Functor (Query e) where
  fmap f q = Query $ \end run -> fmap f (unQuery q end run)

instance Applicative (Query e) where
  pure x = Query $ \_ _ -> pure x
  (<*>) = ap

instance Monad (Query e) where
  return = pure
  left >>= k = Query $ \end run ->
    unQuery left end run >>= \s ->
      unQuery (k s) end run

queryWith :: E.SqlSelect a r => ([r] -> t) -> E.SqlQuery a -> Query e t
queryWith k q = Query $ \_ run -> run (QueryPart q k)

query :: E.SqlSelect a r => E.SqlQuery a -> Query e [r]
query = queryWith id

-- | Like 'single', but does not raise an error and appends @LIMIT 2@ to the query.
querySingle :: E.SqlSelect a r => E.SqlQuery a -> Query e (Maybe r)
querySingle q = do
  rs <- query (q <* E.limit 2)
  pure $ case rs of
    [x] -> Just x
    _ -> Nothing

-- | Check that no results exist. Uses @SELECT ... LIMIT 1@ instead of @NOT EXISTS@.
notExists :: forall e a. E.SqlQuery a -> Query e Bool
notExists q = queryWith null (q >> E.limit 1)

-- | Check if a result exists. Uses @SELECT ... LIMIT 1@ instead of @EXISTS@.
exists :: forall e a. E.SqlQuery a -> Query e Bool
exists q = not <$> notExists q

-- | Use a read-only persistent action as a query.
--
-- Why does it take a @ReaderT@ with a @SqlBackend@? Becuase
-- 'Database.Persist.Sql.getFieldName' and similar functions give one.
rawSql :: P.RawSql t => Reader P.SqlBackend (Text, [P.PersistValue]) -> Query neverFails [t]
rawSql mkParams = Query $ \_ run -> run (RawSqlPart (runReader mkParams) id)

first :: [r] -> Maybe r
first lst = case lst of
  [] -> Nothing
  r : _ -> Just r

withDefault :: forall e r. r -> Maybe r -> Query e r
withDefault def = pure . fromMaybe def

-- | @Nothing@ if the result is empty else @Just r@ where @r@ is the first record.
maybeOne :: forall e r. [r] -> Query e (Maybe r)
maybeOne lst = case lst of
  r : _rs -> pure (Just r)
  [] -> pure Nothing

-- | Get the first record that has this field as this value.
getFirstRecordByField ::
  forall err record field.
  (E.PersistEntity record, E.PersistField field) =>
  E.EntityField record field ->
  field ->
  Query err (Maybe (E.Entity record))
getFirstRecordByField field value = do
  result <- query $ do
    t <- E.from $ E.table @record
    E.where_ (t E.^. field E.==. E.val value)
    pure t
  pure $ first result

unexpected :: HasCallStack => e -> Query e anything
unexpected = unexpectedAt callStack

unexpectedAt :: CallStack -> e -> Query e anything
unexpectedAt cstack e = Query $ \end _ -> end [topOfStack cstack] e
  where
  -- Take the top of the stack, because the whole stack might contain parts of
  -- the Query's stack (cstack')
  topOfStack :: CallStack -> CallStack
  topOfStack = fromCallSiteList . take 1 . getCallStack

exceptionAs :: (e -> e') -> Query e t -> Query e' t
exceptionAs k (Query q) = Query $ \end run -> q (\cstack -> end cstack . k) run

exceptionAt :: ([CallStack] -> [CallStack]) -> Query e t -> Query e t
exceptionAt k (Query q) = Query $ \end run -> q (\cstack -> end (k cstack)) run

--  Discard Query exception
silent :: Query e t -> Query () t
silent = exceptionAs (const ())

withException :: Query e t -> Query void (Either e t)
withException (Query q) = Query $ \_end run ->
  runExceptT (q (\_cstack -> throwE) (lift . run))

withExceptionAndStackTrace :: Query e t -> Query void (Either ([CallStack], e) t)
withExceptionAndStackTrace (Query q) = Query $ \_end run ->
  runExceptT (q (\cstack e -> throwE (cstack, e)) (lift . run))
