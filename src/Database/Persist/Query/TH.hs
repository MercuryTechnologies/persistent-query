{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Avoid restricted alias" -}
{- HLINT ignore "Avoid restricted module" -}
{- HLINT ignore "Avoid throwIO" -}

-- |
-- =
--
-- This module is mostly copied from Database.Persist.Sql.Raw.QQ but extends
-- that quoter with an Input {{x}} substituter, which takes input.x from
-- whatever input is given.
--
-- The adapted portions are licensed under MIT, copyright (c) 2012 Michael
-- Snoyman. See licenses/persistent-qq-MIT.txt for the full upstream notice.
-- Upstream: https://github.com/yesodweb/persistent/tree/f3dab2277585e8fd97d674fb923b9998e431c963/persistent-qq
module Database.Persist.Query.TH
  ( RawSql (..),
    Single (..),
    module Database.Persist.Query.TH,
  )
where

import Control.Arrow (first, second)
import Data.List (intercalate)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Raw.QQ
import Language.Haskell.Meta.Parse
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Database.Persist.Query qualified as DB.Query
import Prelude

data Token
  = Literal String
  | Value String
  | Values String
  | Rows String
  | TableName String
  | ColumnName String
  | Input String
  deriving (Show)

parseHaskell :: (String -> Token) -> String -> String -> [Token]
parseHaskell cons = go
  where
    go a [] = [Literal (reverse a)]
    go a ('\\' : x : xs) = go (x : a) xs
    go a ['\\'] = go ('\\' : a) []
    -- Is it OK to do this? I think so.
    go a ('}' : '}' : xs) = cons (reverse a) : parseStr [] xs
    go a ('}' : xs) = cons (reverse a) : parseStr [] xs
    go a (x : xs) = go (x : a) xs

parseStr :: String -> String -> [Token]
parseStr a [] = [Literal (reverse a)]
parseStr a ('\\' : x : xs) = parseStr (x : a) xs
parseStr a ['\\'] = parseStr ('\\' : a) []
parseStr a ('#' : '{' : xs) = Literal (reverse a) : parseHaskell Value [] xs
parseStr a ('%' : '{' : xs) = Literal (reverse a) : parseHaskell Values [] xs
parseStr a ('*' : '{' : xs) = Literal (reverse a) : parseHaskell Rows [] xs
parseStr a ('^' : '{' : xs) = Literal (reverse a) : parseHaskell TableName [] xs
parseStr a ('@' : '{' : xs) = Literal (reverse a) : parseHaskell ColumnName [] xs
parseStr a ('{' : '{' : xs) = Literal (reverse a) : parseHaskell Input [] xs
parseStr a (x : xs) = parseStr (x : a) xs

interpolateValues :: PersistField a => NonEmpty a -> (String, [[PersistValue]]) -> (String, [[PersistValue]])
interpolateValues xs =
  first (mkPlaceholders values <>)
    . second (NonEmpty.toList values :)
  where
    values = NonEmpty.map toPersistValue xs

interpolateRows :: ToRow a => NonEmpty a -> (String, [[PersistValue]]) -> (String, [[PersistValue]])
interpolateRows xs (sql, vals) =
  (placeholders <> sql, values : vals)
  where
    rows :: NonEmpty (NonEmpty PersistValue)
    rows = NonEmpty.map toRow xs

    n = NonEmpty.length rows
    placeholders = n `timesCommaSeparated` mkPlaceholders (NonEmpty.head rows)
    values = List.concatMap NonEmpty.toList $ NonEmpty.toList rows

mkPlaceholders :: NonEmpty a -> String
mkPlaceholders values = "(" <> n `timesCommaSeparated` "?" <> ")"
  where
    n = NonEmpty.length values

timesCommaSeparated :: Int -> String -> String
timesCommaSeparated n = intercalate "," . replicate n

-- | This is just like the one from sqlQQ except that the expression is always a
-- function, and the {{x}} values will appear as HasField selectors on the
-- sole parameter.
makeExpr :: [Token] -> TH.ExpQ
makeExpr toks =
  TH.lam1E
    (TH.varP (TH.mkName "input"))
    [|
      DB.Query.rawSql $ do
        (sql, vals) <- $(go (TH.varE (TH.mkName "input")) toks)
        pure (Text.pack sql, concat vals)
      |]
  where
    go :: TH.ExpQ -> [Token] -> TH.ExpQ
    go _ [] =
      [|pure (mempty :: String, [])|]
    go i (Literal a : xs) =
      [|first (a <>) <$> $(go i xs)|]
    go i (Value a : xs) = do
      [|(\(str, vals) -> ("?" <> str, [toPersistValue $(reifyExp a)] : vals)) <$> ($(go i xs))|]
    go i (Values a : xs) =
      [|interpolateValues $(reifyExp a) <$> $(go i xs)|]
    go i (Rows a : xs) =
      [|interpolateRows $(reifyExp a) <$> $(go i xs)|]
    go i (ColumnName a : xs) =
      [|
        getFieldName $(reifyExp a) >>= \field ->
          first (Text.unpack field <>) <$> $(go i xs)
        |]
    go i (TableName a : xs) = do
      [|
        getTableName (error "record" :: $(TH.conT (TH.mkName a))) >>= \table ->
          first (Text.unpack table <>) <$> $(go i xs)
        |]
    go i (Input a : xs) = do
      [|(\(str, vals) -> ("?" <> str, [toPersistValue $(TH.getFieldE i a)] : vals)) <$> $(go i xs)|]

reifyExp :: String -> TH.Q TH.Exp
reifyExp s =
  case parseExp s of
    Left e -> TH.reportError e >> [|mempty|]
    Right v -> pure v

makeQQ :: QuasiQuoter
makeQQ =
  QuasiQuoter
    (makeExpr . parseStr [])
    (error "Cannot use qc as a pattern")
    (error "Cannot use qc as a type")
    (error "Cannot use qc as a dec")

sqlFunctionQQ :: QuasiQuoter
sqlFunctionQQ = makeQQ
