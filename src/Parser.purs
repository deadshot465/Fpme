module Parser where
  
import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log $ show $ parse' threeChars "A"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "ABCD"

type ParserState a = Tuple String a

class ParserError e where
  eof :: e

data PError = EOF
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF

type ParserFunction e a = ParserError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParserFunction e a)

instance functorParser :: Functor (Parser e) where
  map f p = Parser \s -> (map f) <$> parse p s

instance applyParser :: Apply (Parser e) where
  apply p1 p2 = Parser \s -> case parse p1 s of
    Left err -> Left err
    Right (Tuple s1 h) -> case parse p2 s1 of
      Left err -> Left err
      Right (Tuple s2 x) -> Right $ Tuple s2 (h x)

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

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char

threeChars :: ∀ e. Parser e String
threeChars = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

count :: ∀ e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count n p | n <= 0 = pure none
          | otherwise = sequence (replicate n p)