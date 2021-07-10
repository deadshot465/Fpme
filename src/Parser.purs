module Parser where
  
import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (uncons)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log "placeholder"

type ParserState a = Tuple String a

data PError = EOF
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

class ParserError e where
  eof :: e

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

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left eof
  Just { head, tail } -> Right $ Tuple tail head