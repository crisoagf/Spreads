{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Spread.Parser where
import Spread.TypesAndVals
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Number
import Data.List (foldl')
import Data.Ix
import Data.Bifunctor

parseCell :: Stream String m t => ParsecT String u m (CellValue a)
parseCell = parseApps <* eof

parseApps :: Stream String m t => ParsecT String u m (CellValue a)
parseApps = between (string "(") (string ")") parseApps <|> (findApps =<< expr `sepBy1` string " ")
 where findApps [] = fail "cell expression"
       findApps [a] = pure a
       findApps (a:b:as) = pure $ foldl' App (App a b) as

expr :: Stream String m t => ParsecT String u m (CellValue a)
expr =
  try (RawValue <$> rawValue) <|>
  -- Deprecated syntax -- try cellRange <|>
  lambda <|>
  variable <|>
  ref <|>
  namedRef <|>
  between (string "(") (string ")") parseApps <?> "cell expression"

rawValue :: Stream String m t => ParsecT String u m (CellRawValue a)
rawValue =
  try (FloatValue <$> fractional) <|>
  try (IntValue <$> int) <|>
  ((StringValue . T.pack) <$> between (string "\"") (string "\"") (many escapedString)) <|>
  (ListValue <$> between (string "[") (string "]") (rawValue `sepBy` string ",")) <|>
  (PosValue <$> (string "@" *> between (string "{") (string "}") ((,) <$> (fromIntegral <$> nat) <* string "," <*> (fromIntegral <$> nat)))) <|>
  (TypeValue <$> (string "#" *> typeValue)) <?> "raw cell value"

escapedString :: Stream String m t => ParsecT String u m Char
escapedString = (noneOf "\\\"" <|> (char '\\' *> oneOf "\\\""))

typeValue :: Stream String m t => ParsecT String u m CellType
typeValue =
  buildExpressionParser [[Prefix ((string "List ") *> pure ListType)], [Infix (try (string " -> ") *> pure FuncType) AssocRight], []] basicType <|>
  basicType <|>
  between (string "(") (string ")") typeValue <?> "A Type"

basicType :: Stream String m t => ParsecT String u m CellType
basicType = 
  (string "Int"    *> pure    IntType) <|>
  (string "Float"  *> pure  FloatType) <|>
  (string "String" *> pure StringType) <|>
  (string "Date"   *> pure   DateType) <|>
  (string "Time"   *> pure   TimeType) <|>
  (string "Pos"    *> pure    PosType) <|>
  (string "Type"   *> pure   TypeType) <|>
  ((VarType . T.pack) <$> ((:) <$> lower <*> many alphaNum)) <?> "basic type"

variable :: Stream String m t => ParsecT String u m (CellValue a)
variable = (VariableValue . T.pack) <$> ((:) <$> lower <*> many alphaNum) <?> "variable"

namedRef :: Stream String m t => ParsecT String u m (CellValue a)
namedRef = (NamedRefValue . T.pack) <$> ((:) <$> upper <*> many alphaNum) <?> "named reference"

ref :: Stream String m t => ParsecT String u m (CellValue a)
ref = RefValue <$> (string "@!" *> between (string "{") (string "}") ((,) <$> (fromIntegral <$> nat) <* string "," <*> (fromIntegral <$> nat)))

lambda :: Stream String m t => ParsecT String u m (CellValue a)
lambda = fmap RawValue $ FuncValue <$> fmap T.pack (string "\\" *> ((:) <$> lower <*> many alphaNum) <* string ":") <*> typeValue <* string " => " <*> expr

{- Old syntax for ranges, deprecated
cellRange :: Stream String m t => ParsecT String u m (CellValue a)
cellRange = string "!" *> (Range <$> try (curry range <$> ((,) <$> (fromIntegral <$> nat) <* string "#" <*> (fromIntegral <$> nat)) <* string "!" <*> ((,) <$> (fromIntegral <$> nat) <* string "#" <*> (fromIntegral <$> nat)))) <?> "range"
-}
