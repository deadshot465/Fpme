module Ch15 where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Profunctor (class Profunctor, dimap)
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ odd 0
  log $ show $ odd 1
  log "------------------------------------"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log "------------------------------------"
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log $ show $ runFoldL addr [1, 2, 3]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ runFoldL sizer ["This", "is", "the", "test"]

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) = f

instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate g) = Predicate (g <<< f)

data Moore s a b = Moore s (s -> b) (s -> a -> s)

{-
  dimap :: ∀ a b c d. (c -> a) -> (b -> d) -> p a b -> p c d
  f :: c -> a
  g :: b -> d
  output :: s -> b
  transition s :: a -> s
-}
instance profunctorMoore :: Profunctor (Moore s) where
  dimap f g (Moore s0 output transition) = Moore s0 (output >>> g) (\s' -> f >>> transition s')

addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: ∀ f s a b. Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s output acc) = output <<< foldl acc s

sizer :: Moore Int String String
sizer = dimap length (\n -> "Size is " <> show n) addr