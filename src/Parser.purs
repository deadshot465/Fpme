module Parser where
  
import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array ((:))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|), fromNonEmpty)
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, singleton, uncons)
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
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3"
  log $ show $ parse' (atMost' 2 alphaNum) "$_$"
  log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3"
  log $ show $ parse' monthFirst "12/31/1999"
  log $ show $ parse' (some' digit) "2343423423abc"
  log $ show $ parse' (many' digit) "_2343423423abc"
  log $ show $ parse' (some' digit) "_2343423423abc"
  log $ show $ parse' ugly "17, some words"
  log $ show $ parse' ugly "5432, some more words1234567890"

type ParserState a = Tuple String a

class ParserError e where
  eof :: e
  invalidChar :: String -> e
  invalidDate :: String -> e

data PError = EOF | InvalidChar String | InvalidDate String
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar msg = InvalidChar msg
  invalidDate msg = InvalidDate msg

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

newtype Year = Year Int
derive instance genericYear :: Generic Year _
instance showYear :: Show Year where
  show = genericShow

newtype Month = Month Int
derive instance genericMonth :: Generic Month _
instance showMonth :: Show Month where
  show = genericShow

newtype Day = Day Int
derive instance genericDay :: Generic Day _
instance showDay :: Show Day where
  show = genericShow

data DateFormat = YearFirst | MonthFirst

type DateParts = 
  { year :: Year
  , month :: Month
  , day :: Day
  , format :: DateFormat
  }

derive instance genericDateFormat :: Generic DateFormat _
instance showDateFormat :: Show DateFormat where
  show = genericShow

{- atMost :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
atMost n p | n <= 0 = pure []
           | otherwise = optional [] $ p >>= \c -> (c : _) <$> atMost (n - 1) p -}

atMost :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMost cons n p | n <= 0 = pure none
                | otherwise = optional none $ p >>= \c -> cons c <$> atMost cons (n - 1) p

atMost' :: forall e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost (:) n p

optional :: ∀ f a. Alt f => Applicative f => a -> f a -> f a
optional x p = p <|> pure x

range :: ∀ e a f. Semigroup (f a) => Traversable f => Unfoldable f => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p | min < 0 || max <= 0 || max < min = pure none
                     | otherwise = count min p >>= \cs -> (cs <> _) <$> atMost cons (max - min) p

range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

constChar' :: ∀ e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (singleton c) (_ == c)

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString

yearFirst :: ∀ e. ParserError e => Parser e DateParts
yearFirst = do
  year <- count' 4 digit <#> digitsToNum >>> Year
  --year <- count' 4 digit >>= \s -> pure $ (fromString s) >>= \i -> pure $ Year i
  constChar '-'
  month <- range' 1 2 digit <#> digitsToNum >>> Month
  constChar '-'
  day <- range' 1 2 digit <#> digitsToNum >>> Day
  pure { year, month, day, format: YearFirst }

monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  year <- Year <<< digitsToNum <$> count' 4 digit
  pure { year, month, day, format: MonthFirst }

date :: ∀ e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst <|> fail (invalidDate "Invalid date.")

some :: ∀ a f m
  . Unfoldable f 
  => Applicative m
  => Lazy (m (f a)) 
  => Alt m 
  => (a -> f a -> f a) 
  -> m a 
  -> m (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p

some' :: ∀ e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty (:) <$> some (:) p

many :: ∀ a f m
  . Unfoldable f 
  => Alt m 
  => Applicative m 
  => Lazy (m (f a)) 
  => (a -> f a -> f a) 
  -> m a 
  -> m (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure none

many' :: ∀ e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many (:) p

instance lazyParser :: Lazy (Parser e a) where
  defer f = Parser \s -> parse (f unit) s

digits :: ∀ e. ParserError e => Parser e String
digits = some' digit

ugly :: ∀ e. ParserError e => Parser e (Array String)
ugly = do
  p1 <- range' 1 4 digit
  constChar ','
  constChar ' '
  p2 <- some' (letter <|> constChar' ' ')
  p3 <- many' digit
  pure [p1, p2, p3]