module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ toCSV person
  log $ show $ toCSV person == CSV "John, 25, Doctor"
  log $ show $ (toCSV person # fromCSV) == Just person
    where
      person = Person { name: FullName "John", age: Age 25, occupation: Doctor }

newtype CSV = CSV String
newtype FullName = FullName String
newtype Age = Age Int

data Occupation = Doctor | Dentist | Lawyer | Unemployed

data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

derive newtype instance eqFullName :: Eq FullName
instance showFullName :: Show FullName where
  show (FullName name) = name

derive newtype instance eqAge :: Eq Age
derive newtype instance showAge :: Show Age

derive instance eqOccupation :: Eq Occupation
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow

derive instance eqPerson :: Eq Person

class ToCSV a where
  toCSV :: a -> CSV

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) = CSV $ show name <> ", " <> show age <> ", " <> show occupation

class ToCSV a <= FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ", ") str of
    [name, age, occupation] -> case fromString age of
      Just age' -> case toOccupation occupation of
        Just occupation' -> Just $ Person
          { name: FullName name
          , age: Age age'
          , occupation: occupation'
          }
        Nothing -> Nothing
      Nothing -> Nothing
    _ -> Nothing

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

-- Alternative implementation
{- instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ", ") str of
    [name, age, occupation] -> do
      age' <- fromString age
      occupation' <- toOccupation occupation
      pure $ Person
        { name: FullName name
        , age: Age age'
        , occupation: occupation'
        }
    _ -> Nothing -}