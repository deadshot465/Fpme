module Ch18 where
  
import Prelude

import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
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
  tellRWS ["test the log"]
  tellRWS ["test the log2", "test the log3"]
  config <- askRWS
  tellRWS ["the config is " <> show config]
  counter <- getRWS
  tellRWS ["old counter is " <> show counter]
  putRWS $ counter + 1
  newCounter <- getRWS
  tellRWS ["new counter is " <> show newCounter]
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

newtype Writer w a = Writer (Tuple a w)

instance functorWriter :: Functor (Writer w) where
  map f (Writer (Tuple a w)) = Writer $ Tuple (f a) w

instance applyWriter :: Semigroup w => Apply (Writer w) where
  apply (Writer (Tuple f w)) (Writer (Tuple a w')) = Writer $ Tuple (f a) (w <> w')

instance applicativeWriter :: Monoid w => Applicative (Writer w) where
  pure x = Writer $ Tuple x mempty

instance bindWriter :: Monoid w => Bind (Writer w) where
  bind (Writer (Tuple a w)) f = f a # \(Writer (Tuple b w')) -> Writer $ Tuple b (w <> w')

instance monadWriter :: Monoid w => Monad (Writer w)

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

tellRWS :: ∀ r w s. w -> RWS r w s Unit
tellRWS w = RWS \{ r, s } -> Tuple unit { r, w, s }

askRWS :: ∀ r w s. Monoid w => RWS r w s r
askRWS = RWS \{ r, s } -> Tuple r { r, w: mempty, s }

getRWS :: ∀ r w s. Monoid w => RWS r w s s
getRWS = RWS \{ r, s } -> Tuple s { r, w: mempty, s }

putRWS :: ∀ r w s. Monoid w => s -> RWS r w s Unit
putRWS s = RWS \{ r } -> Tuple unit { r, w: mempty, s }

newtype WriterT w m a = WriterT (m (Tuple a w))

runWriterT :: ∀ w m a. WriterT w m a -> m (Tuple a w)
runWriterT (WriterT mx) = mx

instance functorWriterT :: Functor m => Functor (WriterT w m) where
  map f (WriterT mx) = WriterT $ mx <#> \(Tuple x w) -> Tuple (f x) w

instance applyWriterT :: (Monoid w, Monad m) => Apply (WriterT w m) where
  apply (WriterT mf) (WriterT mx) = WriterT $ do
    Tuple x w <- mx
    Tuple f w' <- mf
    pure $ Tuple (f x) (w <> w')

instance applicativeWriterT :: (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure x = WriterT $ pure $ Tuple x mempty

instance bindWriterT :: (Monoid w, Monad m) => Bind (WriterT w m) where
  bind (WriterT mx) f = WriterT $ do
    Tuple x w <- mx
    Tuple y w' <- runWriterT $ f x
    pure $ Tuple y (w <> w')

instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)

newtype ReaderT r m a = ReaderT (r -> m a)

runReaderT :: ∀ r m a. ReaderT r m a -> r -> m a
runReaderT (ReaderT mf) = mf

instance functorReaderT :: Functor m => Functor (ReaderT r m) where
  map f (ReaderT mg) = ReaderT $ map f <<< mg

instance applyReaderT :: Apply m => Apply (ReaderT r m) where
  apply (ReaderT mf) (ReaderT mg) = ReaderT (\r -> mf r <*> mg r)

instance applicativeReaderT :: Monad m => Applicative (ReaderT r m) where
  --pure = ReaderT <<< const <<< pure
  pure = lift <<< pure

instance bindReaderT :: Monad m => Bind (ReaderT r m) where
  bind (ReaderT mf) mg = ReaderT $ \r -> mf r >>= \a -> runReaderT (mg a) r

instance monadReaderT :: Monad m => Monad (ReaderT r m)

instance monadTransReaderT :: MonadTrans (ReaderT r) where
  lift = ReaderT <<< const

instance monadAskReaderT :: Monad m => MonadAsk r (ReaderT r m) where
  ask = ReaderT pure

instance monadTellReaderT :: MonadTell w m => MonadTell w (ReaderT r m) where
  tell = lift <<< tell