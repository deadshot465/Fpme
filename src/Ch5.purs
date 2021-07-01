module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, max, negate, otherwise, show, (+), (-), (/=), (<), (<<<), (>>>), (==), (>), (>=))

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log "Null --"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log "Snoc --"
  log $ show $ snoc (1 : 2 : Nil) 3
  log "Length --"
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show $ length' $ 1 : 2 : 3 : Nil
  log "Head --"
  log $ show (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log "Tail --"
  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc" : "123" : Nil)
  log "Last --"
  log $ show $ last (Nil :: List Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log "Init --"
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log "Uncons --"
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log "Index --"
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil) (-99)
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log "Find Index --"
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log "Find Last Index --"
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log "Reverse --"
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log "Concat --"
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log "Filter --"
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ filter' (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log "CatMaybes --"
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log "Range --"
  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log $ show $ range' 1 10
  log $ show $ range' 3 (-3)
  log "Take --"
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
  log "Drop --"
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)
  log "TakeWhile --"
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log "DropWhile --"
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  log "TakeEnd --"
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)
  log "DropEnd --"
  log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ dropEnd 10 (1 : Nil)
  log "Zip --"
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)
  log "Unzip --"
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip (Nil :: List (Tuple Unit Unit))

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped x f = f x

infixl 0 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

length :: ∀ a. List a -> Int
length Nil = 0
length (_ : xs) = 1 + length xs

length' :: ∀ a. List a -> Int
length' l = go l 0
  where
    go :: ∀ b. List b -> Int -> Int
    go Nil n = n
    go (_ : xs') n = go xs' (n + 1)

head :: ∀ a. List a -> Maybe a
head (x : _) = Just x
head _ = Nothing

tail :: ∀ a. List a -> Maybe (List a)
tail (_ : xs) = Just xs
tail _ = Nothing

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l
  where
    go :: ∀ b. List b -> List b
    go Nil = Nil
    go (_ : Nil) = Nil
    go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ i | i < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil = Nothing
findIndex pred l = go 0 l
  where
    go :: Int -> List a -> Maybe Int
    go acc (x : _) | pred x = Just acc
    go _ Nil = Nothing
    go acc (_ : xs) = go (acc + 1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex pred l = go 0 l Nothing
  where
    go :: Int -> List a -> Maybe Int -> Maybe Int
    go _ Nil result = result
    go acc (x : xs) result | pred x = go (acc + 1) xs (Just acc)
                           | otherwise = go (acc + 1) xs result

reverse :: List ~> List
reverse Nil = Nil
reverse l = go l Nil
  where
    go (x : xs) acc = go xs (x : acc)
    go Nil acc = acc

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter pred (x : xs) | pred x = x : filter pred xs
                     | otherwise = filter pred xs

filter' :: ∀ a. (a -> Boolean) -> List a -> List a
filter' pred = reverse <<< go Nil
  where
    go acc Nil = acc
    go acc (x : xs) = if pred x then go (x : acc) xs else go acc xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes ((Just x) : xs) = x : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs

range :: Int -> Int -> List Int
range start end | end > start = start : range (start + 1) end
                | end < start = start : range (start - 1) end
                | otherwise = singleton end

range' :: Int -> Int -> List Int
range' start end = go Nil end start
  where
    go acc start' end' | start' == end' = end' : acc
                       | otherwise = go (start' : acc) (start' + step) end'
    step = if end > start then (-1) else 1

take :: ∀ a. Int -> List a -> List a
take n = reverse <<< go Nil (max 0 n)
  where
    go :: List a -> Int -> List a -> List a
    go acc 0 _ = acc
    go acc _ Nil = acc
    go acc n' (x : xs) = go (x : acc) (n' - 1) xs

drop :: ∀ a. Int -> List a -> List a
drop 0 l = l
drop _ Nil = Nil
drop n (_ : xs) = drop (n - 1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile pred = reverse <<< go Nil pred
  where
    go acc _ Nil = acc
    go acc pred' (x : xs) = if pred' x then go (x : acc) pred' xs else acc

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs) = if pred x then dropWhile pred xs else l

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n = go >>> snd
  where
    go Nil = Tuple 0 Nil
    go (x : xs) = go xs # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then x : nl else nl

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = go >>> snd
  where
    go Nil = Tuple 0 Nil
    go (x : xs) = go xs # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then nl else x : nl

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip l = reverse <<< go Nil l
  where
    go acc Nil _ = acc
    go acc _ Nil = acc
    go acc (x : xs) (y : ys) = go (Tuple x y : acc) xs ys

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : ts) = unzip ts # \(Tuple xs ys) -> Tuple (x : xs) (y : ys)