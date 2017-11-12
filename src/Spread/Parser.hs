{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Spread.Parser where
import Spread.TypesAndVals
import qualified Data.Text.Lazy as T
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Number
import Data.List (foldl')
import Data.Ix
import Data.Bifunctor
import Morte.Core

parseCell :: Stream String m t => ParsecT String u m CellExpr
parseCell = parseApps <* eof

parseApps :: Stream String m t => ParsecT String u m CellExpr
parseApps = between (string "(") (string ")") parseApps <|> (findApps =<< expr `sepBy1` string " ")
 where findApps [] = fail "cell expression"
       findApps [a] = pure a
       findApps (a:b:as) = pure $ foldl' App (App a b) as

expr :: Stream String m t => ParsecT String u m CellExpr
expr =
  try (Embed . RawValue <$> rawValue) <|>
  lambda <|>
  variable <|>
  ref <|>
  namedRef <|>
  between (string "(") (string ")") parseApps <?> "cell expression"

rawValue :: Stream String m t => ParsecT String u m CellRawValue
rawValue =
  try (FloatValue <$> fractional) <|>
  try (IntValue <$> int) <|>
  ((StringValue . T.pack) <$> between (string "\"") (string "\"") (many escapedString)) <|>
  (flip ListValue <$> between (string "[") (string ":") ((fmap Embed rawValue) `sepBy` string ",") <*> (typeValue <* string "]")) <|>
  (PosValue <$> (string "@" *> between (string "{") (string "}") ((,) <$> (fromIntegral <$> nat) <* string "," <*> (fromIntegral <$> nat)))) <|>
  (TypeValue <$> (string "#" *> typeValue)) <?> "a raw cell value"

escapedString :: Stream String m t => ParsecT String u m Char
escapedString = (noneOf "\\\"" <|> (char '\\' *> oneOf "\\\""))

typeValue :: Stream String m t => ParsecT String u m CellType
typeValue =
  between (string "(") (string ")") (buildExpressionParser [[Infix (try (string " -> ") *> pure (Pi "")) AssocRight], [Infix (many1 (char ' ') *> pure App) AssocLeft]] ((Embed <$> basicType) <|>
    ((Var . flip V 0  . T.pack) <$> ((:) <$> lower <*> many alphaNum))) <|>
    (string "*" >> pure (Const Star))) <|>
  (Embed <$> basicType) <|>
  ((Var . flip V 0  . T.pack) <$> ((:) <$> lower <*> many alphaNum)) <|>
  (string "*" >> pure (Const Star)) <|>
  between (string "(") (string ")") typeValue <?> "a type"

basicType :: Stream String m t => ParsecT String u m RawType
basicType = 
  (string "Int"    *> pure    Int) <|>
  (string "Float"  *> pure  Float) <|>
  (string "String" *> pure String) <|>
  (string "Date"   *> pure   Date) <|>
  (string "Time"   *> pure   Time) <|>
  (string "Pos"    *> pure    Pos) <|>
  (string "List"   *> pure   List) <?> "basic type"

variable :: Stream String m t => ParsecT String u m CellExpr
variable = (Var . flip V 0 . T.pack) <$> ((:) <$> lower <*> many alphaNum) <?> "variable"

namedRef :: Stream String m t => ParsecT String u m CellExpr
namedRef = (Embed . NamedRefValue . T.pack) <$> ((:) <$> upper <*> many alphaNum) <?> "named reference"

ref :: Stream String m t => ParsecT String u m CellExpr
ref = (Embed . RefValue) <$> (string "@!" *> between (string "{") (string "}") ((,) <$> (fromIntegral <$> nat) <* string "," <*> (fromIntegral <$> nat)))

lambda :: Stream String m t => ParsecT String u m CellExpr
lambda = do
  string "\\"
  var <- fmap T.pack ((:) <$> lower <*> many alphaNum)
  string ":"
  t <- fmap ((Embed . RawValue . TypeValue . Embed) =<<) (try typeValue)
  string " => "
  e <- expr
  pure $ Lam var t e

