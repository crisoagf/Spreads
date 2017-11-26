{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Spread.Parser (exprFromText, exprFromText') where
import Prelude hiding (pi)
import Spread.TypesAndVals
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Bifunctor
import Morte.Core
import Data.Functor.Identity
import Data.Time
import Control.Monad

sc :: Ord e => ParsecT e Text m ()
sc = space

symbol :: Ord e => Text -> ParsecT e Text m Text
symbol = L.symbol sc

signed :: (Num x, Ord e) => ParsecT e Text m x -> ParsecT e Text m x
signed = L.signed sc

date :: Ord e => ParsecT e Text m Day
date = (fromGregorian <$> (read <$> replicateM 4 digitChar) <* string "-" <*> (read <$> replicateM 2 digitChar) <* string "-" <*> (read <$> replicateM 2 digitChar) <* sc) <?> "date"

diffTime :: Ord e => ParsecT e Text m NominalDiffTime
diffTime = (\ x y z -> 3600 * fromIntegral (x :: Int) + 60 * fromIntegral (y :: Int) + realToFrac (z :: Double)) <$> decimal <* string ":" <*> decimal <* string ":" <*> float

time :: Ord e => ParsecT e Text m UTCTime
time = (\ x y -> addUTCTime y (UTCTime x 0)) <$> date <*> diffTime <* optional (symbol "UTC")

decimal :: (Ord e, Integral n) => ParsecT e Text m n
decimal = L.decimal <* sc

float :: (Ord e, RealFloat x) => ParsecT e Text m x
float = L.float <* sc

cellIndex :: (Ord e) => ParsecT e Text m CellIndex
cellIndex = CellIndex <$> ((,) <$> decimal <* symbol "," <*> decimal)

exprFromText :: String -> Text -> Either (CellError CellIndex) (CellExpr CellIndex)
exprFromText = exprFromText' cellIndex

exprFromText' :: ParsecT X Text Identity i -> String -> Text -> Either (CellError i) (CellExpr i)
exprFromText' indParser l t = bimap PE id $ parse (parseCell indParser <* eof) l t

parseCell :: Ord e => ParsecT e Text m i -> ParsecT e Text m (CellExpr i)
parseCell indParser = expr (baseValue indParser) <?> "cell expression"

expr :: Ord e => ParsecT e Text m v -> ParsecT e Text m (Expr v)
expr emb =
  makeExprParser (between (symbol "(") (symbol ")") (expr emb) <|> zeroarySymbols emb) [[InfixL (pure App)],[Prefix (manyLambda (expr emb))],[Prefix (manyPi (expr emb))]] <?> "cell expression"

zeroarySymbols :: Ord e => ParsecT e Text m v -> ParsecT e Text m (Expr v)
zeroarySymbols emb =
  (symbol "*" >> pure (Const Star)) <|>
  variable <|>
  (Embed <$> emb) <?> "cell value"

manyLambda :: Ord e => ParsecT e Text m (Expr v) -> ParsecT e Text m (Expr v -> Expr v)
manyLambda exprV = foldr1 (.) <$> some (lambda exprV)

manyPi :: Ord e => ParsecT e Text m (Expr v) -> ParsecT e Text m (Expr v -> Expr v)
manyPi exprV = foldr1 (.) <$> some (pi exprV)

baseValue :: Ord e => ParsecT e Text m i -> ParsecT e Text m (CellValue i)
baseValue indParser =
  try (RawValue <$> rawValue indParser) <|>
  ref indParser <|>
  namedRef <|>
  list indParser <?> "cell value"

rawValue :: Ord e => ParsecT e Text m i -> ParsecT e Text m (CellRawValue i)
rawValue indParser = (
  try (TimeValue <$> time) <|>
  try (DateValue <$> date) <|>
  try (TimeDiffValue <$> diffTime) <|>
  try (FloatValue <$> signed float) <|>
  try (IntValue <$> signed decimal) <|>
  ((StringValue . T.pack) <$> between (string "\"") (symbol "\"") (many escapedText)) <|>
  (PosValue <$> (symbol "@" *> between (symbol "{") (symbol "}") indParser)) <|>
  (TypeValue <$> (symbol "#" *> basicType))
  ) <?> "a raw cell value"

list :: Ord e => ParsecT e Text m i -> ParsecT e Text m (CellValue i)
list indParser = (flip ListValue <$> between (symbol "[") (symbol ":") ((parseCell indParser) `sepBy` symbol ",") <*> (parseCell indParser <* symbol "]")) <?> "a list expression"

escapedText :: Ord e => ParsecT e Text m Char
escapedText = let shouldNotBeConsumed = "\\\"" :: String
                  escapes = "\\\"" :: String in
                   (noneOf shouldNotBeConsumed <|> (char '\\' *> oneOf escapes))

basicType :: Ord e => ParsecT e Text m RawType
basicType = 
  (symbol "Int"      *> pure      Int) <|>
  (symbol "Float"    *> pure    Float) <|>
  (symbol "String"   *> pure   String) <|>
  (symbol "Date"     *> pure     Date) <|>
  (symbol "Time"     *> pure     Time) <|>
  (symbol "TimeDiff" *> pure TimeDiff) <|>
  (symbol "Pos"      *> pure      Pos) <|>
  (symbol "List"     *> pure     List) <?> "basic type"

variable :: Ord e => ParsecT e Text m (Expr i)
variable = (Var . flip V 0) <$> var <?> "variable"

var :: Ord e => ParsecT e Text m Text
var = T.pack <$> ((((:) <$> lowerChar <*> many alphaNumChar)) <* sc) <|> symbol "_"

namedRef :: Ord e => ParsecT e Text m (CellValue i)
namedRef = (NamedRefValue . T.pack) <$> ((:) <$> upperChar <*> many alphaNumChar <* sc) <?> "named reference"

ref :: Ord e => ParsecT e Text m i -> ParsecT e Text m (CellValue i)
ref indParser = RefValue <$> (symbol "@!" *> between (symbol "{") (symbol "}") indParser) <* sc

lambda :: Ord e => ParsecT e Text m (Expr v) -> ParsecT e Text m (Expr v -> Expr v)
lambda exprV = uncurry Lam <$> (litLambda *> varAndType exprV) <* litArrow

varAndType :: Ord e => ParsecT e Text m (Expr v) -> ParsecT e Text m (Text, Expr v)
varAndType exprV = ((,) <$> var <*> (symbol ":" *> exprV)) <|> between (symbol "(") (symbol ")") (varAndType exprV)

litLambda :: Ord e => ParsecT e Text m Text
litLambda = symbol "\\" <|> symbol "λ"

litArrow :: Ord e => ParsecT e Text m Text
litArrow = symbol "->" <|> symbol "→"

pi :: Ord e => ParsecT e Text m (Expr v) -> ParsecT e Text m (Expr v -> Expr v)
pi exprV = uncurry Pi <$> (litPi *> varAndType exprV) <* litArrow

litPi :: Ord e => ParsecT e Text m Text
litPi = symbol "|~|" <|> symbol "Π" <|> symbol "∀"

