module Ch21 where
  
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.State (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log "Helo"

newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: ∀ s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT mf) = mf

instance Monad m => Functor (StateT s m) where
  map f (StateT mg) = StateT \s -> mg s <#> \(Tuple a s') -> Tuple (f a) s'

instance Monad m => Apply (StateT s m) where
  apply (StateT mf) (StateT mg) = StateT \s -> do
    Tuple f s' <- mf s
    Tuple a s'' <- mg s'
    pure $ Tuple (f a) s''

instance Monad m => Applicative (StateT s m) where
  pure x = StateT (\s -> pure $ Tuple x s)

instance Monad m => Bind (StateT s m) where
  bind (StateT mf) mg = StateT \s -> do
    Tuple a s' <- mf s
    runStateT (mg a) s'

instance Monad m => Monad (StateT s m)

instance MonadTrans (StateT s) where
  lift mx = StateT \s -> mx <#> \x -> Tuple x s

instance MonadAsk r m => MonadAsk r (StateT s m) where
  ask = lift ask

instance MonadTell w m => MonadTell w (StateT s m) where
  tell = lift <<< tell

instance Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

instance MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError 