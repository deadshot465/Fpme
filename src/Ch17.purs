module Ch17 where
  
import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ (+) <$> Just 21 <*> Just 21
  log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
  log $ show $ pure (+) <*> Just 17 <*> Just 25
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  apply (Just f) x = f <$> x
  apply Nothing _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

data Either a b = Left a | Right b
derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance functorEither :: Functor (Either a)
derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right x) = Right $ g x

instance applyEither :: Apply (Either a) where
  apply (Left y) _ = Left y
  apply (Right f) x = f <$> x

instance applicativeEither :: Applicative (Either a) where
  pure = Right

newtype Validation err result = Validation (Either err result)
derive newtype instance functorValidation :: Functor (Validation a)
derive newtype instance bifunctorValidation :: Bifunctor Validation
derive newtype instance eqValidation :: (Eq a, Eq b) => Eq (Validation a b)
derive newtype instance ordValidation :: (Ord a, Ord b) => Ord (Validation a b)
derive instance genericValidation :: Generic (Validation a b) _

instance applyValidation :: Semigroup err => Apply (Validation err) where
  apply (Validation (Left err1)) (Validation (Left err2)) = Validation $ Left $ err1 <> err2
  apply (Validation (Left err)) _ = Validation $ Left err
  apply (Validation (Right f)) x = f <$> x

instance applicativeValidation :: Semigroup err => Applicative (Validation err) where
  pure = Validation <<< Right

instance showValidation :: (Show a, Show b) => Show (Validation a b) where
  show = genericShow

newtype Age = Age Int
newtype FullName = FullName String

derive newtype instance showAge :: Show Age
derive newtype instance showFullName :: Show FullName

type FamilyAgesRow r = ( fatherAge :: Age, motherAge :: Age, childAge :: Age | r )
type FamilyNamesRow r = ( fatherName :: FullName, motherName :: FullName, childName :: FullName | r )

newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ()) }
newtype FamilyAges = FamilyAges { | FamilyAgesRow () }

derive instance genericFamilyAges :: Generic FamilyAges _
instance showFamilyAges :: Show FamilyAges where
  show = genericShow

newtype LowerAge = LowerAge Int
newtype UpperAge = UpperAge Int

validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
validateAge (LowerAge low) (UpperAge high) actualAge@(Age age) who | age < low = Validation (Left [who <> " is too young."])
                                                                   | age > high = Validation (Left [who <> " is too old."])
                                                                   | otherwise = Validation (Right $ actualAge)

createFamilyAges :: { | FamilyAgesRow () } -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge, motherAge, childAge } = FamilyAges <$>
  ({ fatherAge: _, motherAge: _, childAge: _ } <$>
  (validateAge (LowerAge 1) (UpperAge 18) fatherAge "Father") <*> 
  (validateAge (LowerAge 1) (UpperAge 18) motherAge "Mother") <*> 
  (validateAge (LowerAge 1) (UpperAge 18) childAge "Child"))