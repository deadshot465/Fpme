module Ch11 where

import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.List (List(..), singleton, (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, class Semiring, Unit, add, discard, negate, otherwise, show, zero, ($), (+), (>), (<>), (<<<))

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log $ show $ findMax' (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax' ("a" : "bbb" : "c" : Nil)
  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
  log $ show $ findMaxNE' (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE' (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
  log $ show $ sum (1 : 2 : 3 : Nil)
  log $ show $ sum' (1 : 2 : 3 : Nil)
  log $ show $ toList (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
  log $ show $ sum' (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))

reverse :: ∀ f a. Foldable f => f a -> List a
reverse = foldl (\acc x -> x : acc) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y | x > y = x
        | otherwise = y

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil = Nothing
findMax l@(x : _) = Just $ go x l
  where
    go :: a -> List a -> a
    go acc Nil = acc
    go acc (x' : xs') = go (max acc x') xs'

findMax' :: ∀ a. Ord a => List a -> Maybe a
findMax' Nil = Nothing
findMax' l@(x : _) = Just $ foldl max x l

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList (NonEmpty first l)) = foldl max first l

findMaxNE' :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE' (NonEmptyList ne) = foldl1 max ne

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 f (x :| xs) = foldl f x xs

sum :: List Int -> Int
sum = go 0
  where
    go acc Nil = acc
    go acc (x : xs) = go (x + acc) xs

sum' :: ∀ f a. Foldable f => Semiring a => f a -> a
sum' = foldl add zero

data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf x) = singleton x
toList (Node lt rt) = toList lt <> toList rt

instance foldableTree :: Foldable Tree where
  foldr f acc = foldr f acc <<< toList
  foldl f acc = foldl f acc <<< toList
  foldMap f = foldMap f <<< toList