module Ch21 where
  
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except.Trans as E
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.State.Trans as S
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
import Control.Monad.Writer.Trans as W
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console

test :: Effect Unit
test = do
  result1 <- runApp 0 app
  Console.log $ show result1
  result2 <- runApp 99 app
  Console.log $ show result2

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

type AppStack e w s a = E.ExceptT e (W.WriterT w (S.StateT s Effect)) a

type AppM = AppStack String String Int Unit

type StackResult = Tuple (Tuple (Either String Unit) String) Int

type AppEffects =
  { log :: String
  , state :: Int
  , result :: Maybe Unit
  }

type AppResult = Tuple (Maybe String) AppEffects

runApp :: Int -> AppM -> Effect AppResult
runApp st = E.runExceptT >>> W.runWriterT >>> flip S.runStateT st >>> (results <$> _)

results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) = Tuple (Just err) ({ log: l, state: s, result: Nothing })
results (Tuple (Tuple (Right res) l) s) = Tuple Nothing ({ log: l, state: s, result: Just res })

app :: AppM
app = do
  log "Starting the app..."
  n <- get
  when (n == 0) $ void $ throwError "We cannot have a 0 state!"
  put $ n + 1
  log "Incremented state."
  pure unit

log :: ∀ m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"