module Ch18 where
  
import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

type Counter = Int
type Config = { debugModeOn :: Boolean }

test :: Effect Unit
test = do
  log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0 }

rwsTest :: RWS Config (Array String) Counter Unit
rwsTest = do
  tell ["test the log"]
  tell ["test the log2", "test the log3"]
  config <- ask
  tell ["the config is " <> show config]
  counter <- get
  tell ["old counter is " <> show counter]
  put $ counter + 1
  newCounter <- get
  tell ["new counter is " <> show newCounter]
  pure unit

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

type RWSResult r w s = 
  { r :: r
  , w :: w
  , s :: s
  }

newtype RWS r w s a = RWS (RWSResult r w s -> Tuple a (RWSResult r w s))

instance functorRWS :: Functor (RWS r w s) where
  map f (RWS g) = RWS \rws -> g rws # \(Tuple a state) -> Tuple (f a) state

instance applyRWS :: Monoid w => Apply (RWS r w s) where
  apply (RWS f) (RWS g) = RWS \rws -> g rws # \(Tuple a state@{ w }) -> f state # \(Tuple ab state'@{ w: w' }) -> Tuple (ab a) state'{ w = w <> w' }

instance applicativeRWS :: Monoid w => Applicative (RWS r w s) where
  pure x = RWS \{ r, s } -> Tuple x { r, w: mempty, s }

instance bindRWS :: Monoid w => Bind (RWS r w s) where
  bind (RWS f) g = RWS \rws -> f rws # \(Tuple a state@{ w }) -> runRWS (g a) state # \(Tuple b state'@{ w: w' }) -> Tuple b state'{ w = w <> w' }

instance monadRWS :: Monoid w => Monad (RWS r w s)

runRWS :: ∀ r w s a. RWS r w s a -> RWSResult r w s -> Tuple a (RWSResult r w s)
runRWS (RWS f) = f

tell :: ∀ r w s. w -> RWS r w s Unit
tell w = RWS \{ r, s } -> Tuple unit { r, w, s }

ask :: ∀ r w s. Monoid w => RWS r w s r
ask = RWS \{ r, s } -> Tuple r { r, w: mempty, s }

get :: ∀ r w s. Monoid w => RWS r w s s
get = RWS \{ r, s } -> Tuple s { r, w: mempty, s }

put :: ∀ r w s. Monoid w => s -> RWS r w s Unit
put s = RWS \{ r } -> Tuple unit { r, w: mempty, s }