module Ch19 where
  
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ Just (_ * 10) <*> Just 20
  log $ show $ Just (_ * 10) <*> pure 20
  log $ show $ Just 20 >>= pure <<< (_ * 10)
  log $ show $ do
    x <- Just 20
    let y = x * 10
    pure y
  log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y * 10
  log $ show $ do
    _ <- Just 20
    y <- Nothing
    pure $ y * 10
  log "-------------------------------------"
  log $ show $ Right (_ * 10) <*> (Right 20 :: Either Unit _)
  log $ show $ Right (_ * 10) <*> (pure 20 :: Either Unit _)
  log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
  log $ show $ do
    x <- Right 20 :: Either Unit _
    let y = x * 10
    pure y
  log $ show $ Right 20 >>= const (Left "error") >>= \y -> Right $ y * 10
  log $ show $ do
    _ <- Right 20 :: Either String _
    y <- Left "error"
    pure $ y * 10

data Maybe a = Nothing | Just a
derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

instance bindMaybe :: Bind Maybe where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance monadMaybe :: Monad Maybe

data Either a b = Left a | Right b
derive instance genericEither :: Generic (Either a b) _
derive instance functorEither :: Functor (Either a)

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance applyEither :: Apply (Either a) where
  apply (Left x) _ = Left x
  apply (Right f) x = f <$> x

instance applicativeEither :: Applicative (Either a) where
  pure = Right

instance bindEither :: Bind (Either a) where
  bind (Left x) _ = Left x
  bind (Right x) f = f x

instance monadEither :: Monad (Either a)