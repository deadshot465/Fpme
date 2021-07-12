module Ch18 where
  
import Prelude

import Data.Tuple (Tuple(..))

newtype Reader r a = Reader (r -> a)

runReader :: ∀ r a. Reader r a -> r -> a
runReader (Reader f) = f

instance functorReader :: Functor (Reader r) where
  map f (Reader x) = Reader (f <<< x)

instance applyReader :: Apply (Reader r) where
  apply (Reader f) (Reader x) = Reader \r -> f r $ x r

instance applicativeReader :: Applicative (Reader r) where
  pure = Reader <<< const

instance bindReader :: Bind (Reader r) where
  bind (Reader x) f = Reader \r -> runReader (x r # f) r

instance monadReader :: Monad (Reader r)

newtype State s a = State (s -> Tuple a s)

runState :: ∀ s a. State s a -> s -> Tuple a s
runState (State f) = f

instance functorState :: Functor (State s) where
  map f (State x) = State \s -> x s # \(Tuple a s') -> Tuple (f a) s'

instance applyState :: Apply (State s) where
  apply (State f) (State x) = State \s -> f s # \(Tuple g s') -> x s' # \(Tuple a s'') -> Tuple (g a) s''

instance applicativeState :: Applicative (State s) where
  pure x = State \s -> Tuple x s

instance bindState :: Bind (State s) where
  bind (State x) f = State \s -> x s # \(Tuple a s') -> runState (f a) s'

instance monadState :: Monad (State s)