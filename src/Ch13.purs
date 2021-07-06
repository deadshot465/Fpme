module Ch13 where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Functor, class Show, Unit, discard, identity, map, show, ($), (*), (/), (<<<), (<>), (==))

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit Int)
  log $ show $ (_ / 2) <$> Left "error reason"
  log $ show $ (_ / 2) <$> Tuple 10 20
  log $ show $ (_ / 2) <$> Triple 10 20 40
  log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
  log $ show $ "Maybe Identity for Just 10: " <> show ((identity <$> Just 10) == Just 10)
  log $ show $ "Maybe Identity for Just \"abc\": " <> show ((identity <$> (Just "abc")) == Just "abc")
  let g x = x * 2
      f x = x * 3
  log $ show $ "Maybe Composition for Nothing: " <> show ((map (g <<< f) Nothing) == (map g <<< map f) Nothing)
  log $ show $ "Maybe Composition for Just: " <> show ((map (g <<< f) (Just 60)) == (map g <<< map f) (Just 60))
  log $ show $ rmap (_ * 2) $ Left "error reason"
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
  log $ show $ lmap toUpper $ Right 10
  log $ show $ rmap (_ * 2) $ Tuple 80 40
  log $ show $ lmap (_ / 2) $ Tuple 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40
  log $ show $ rmap (_ * 2) $ Triple 99 80 40
  log $ show $ lmap (_ / 2) $ Triple 99 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Triple 99 80 40

data Maybe a = Nothing | Just a

derive instance eqMaybe :: Eq a => Eq (Maybe a)

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

infixl 4 map as <$>

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left err) = Left err
  map f (Right x) = Right $ f x

data Tuple a b = Tuple a b

derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple x y) = Tuple x (f y)

data Triple a b c = Triple a b c

derive instance genericTriple :: Generic (Triple a b c) _
instance showTriple :: (Show a, Show b, Show c) => Show (Triple a b c) where
  show = genericShow

instance functorTriple :: Functor (Triple a b) where
  map f (Triple x y z) = Triple x y (f z)

class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right y) = Right $ g y

rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity

lmap :: ∀ f a b c. Bifunctor f => (a -> b) -> f a c -> f b c
lmap f = bimap f identity

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

instance bifunctorTriple :: Bifunctor (Triple a) where
  bimap f g (Triple x y z) = Triple x (f y) (g z)