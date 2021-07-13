module Parser where
  
import Prelude

import Control.Alt (class Alt, (<|>))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoCharsB "ABC"
  log $ show $ parse' threeCharsB "ABC"
  log $ show $ parse' threeCharsB "A"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "ABCD"
  log $ show $ parse' (count' 3 digit) "123456"
  log $ show $ parse' (count' 3 digit) "abc456"
  log $ show $ parse' (count' 4 letter) "Freddy"
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
  log $ show $ parse' (count' 10 alphaNum) "######"

type ParserState a = Tuple String a

class ParserError e where
  eof :: e
  invalidChar :: String -> e

data PError = EOF | InvalidChar String
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar msg = InvalidChar msg

type ParserFunction e a = ParserError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParserFunction e a)

instance functorParser :: Functor (Parser e) where
  map f p = Parser \s -> (map f) <$> parse p s

instance bindParser :: Bind (Parser e) where
  {- bind p f = Parser \s -> do
    Tuple s1 x <- parse p s
    parse (f x) s1 -}
  --bind p f = Parser \s -> parse p s >>= \(Tuple s1 x) -> parse (f x) s1
  bind p f = Parser (parse p >=> \(Tuple s1 x) -> parse (f x) s1)

instance monadParser :: Monad (Parser e)

instance applyParser :: Apply (Parser e) where
  {- apply p1 p2 = Parser \s -> case parse p1 s of
    Left err -> Left err
    Right (Tuple s1 h) -> case parse p2 s1 of
      Left err -> Left err
      Right (Tuple s2 x) -> Right $ Tuple s2 (h x) -}
  -- apply = ap
  apply p1 p2 = Parser \s -> do
    Tuple s1 h <- parse p1 s
    Tuple s2 x <- parse p2 s1
    pure $ Tuple s2 (h x)

instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser \s -> pure $ Tuple s x

parse :: ∀ e a. Parser e a -> ParserFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParserFunction PError a
parse' = parse

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left eof
  Just { head, tail } -> Right $ Tuple tail head

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = char >>= \c1 -> char >>= \c2 -> pure $ (Tuple c1 c2)

threeCharsA :: ∀ e. Parser e String
threeCharsA = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

threeCharsB :: ∀ e. Parser e String
threeCharsB = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [c1, c2, c3]

count :: ∀ e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count n p | n <= 0 = pure none
          | otherwise = sequence (replicate n p)

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy msg pred = char >>= \c -> if pred c then pure c else fail $ invalidChar msg

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

instance altParser :: Alt (Parser e) where
  alt p1 p2 = Parser \s -> case parse p1 s of
    Left _ -> parse p2 s
    Right x -> Right x

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p