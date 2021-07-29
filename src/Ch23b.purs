module Ch23b where
  
import Prelude

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

test :: Effect Unit
test = launchAff_ do
  bus <- Bus.make
  let forkFiberM = runFiberM bus
  forkFiberM logger
  forkFiberM $ randomGenerator (_ > 0.5)
  forkFiberM $ randomGenerator (_ < 0.5)
  forkFiberM $ randomGenerator (_ > 0.1)

type Config = { bus :: BusRW String }
type State = { count :: Int }
type FiberM a = ReaderT Config (StateT State Aff) a

{- randomAff :: Aff Number
randomAff = makeAff \cb -> do
  n <- random
  cb $ Right n
  pure nonCanceler -}

randomAff :: Aff Number
randomAff = liftEffect random

runFiberM :: BusRW String -> FiberM Unit -> Aff Unit
runFiberM bus = void <<< forkAff <<< flip runStateT { count: 10 } <<< flip runReaderT { bus }

logger :: FiberM Unit
logger = forever do
  { bus } <- ask
  s <- liftAffToFiberM $ Bus.read bus
  log $ "Logger: " <> s

randomGenerator :: (Number -> Boolean) -> FiberM Unit
randomGenerator pred = do
  { count } <- get
  unless (count <= 0) do
    { bus } <- ask
    liftAffToFiberM do
      n <- delayRandom
      when (pred n) $ Bus.write "Message" bus
    modify_ _ { count = count - 1 }
    randomGenerator pred

liftAffToFiberM :: Aff ~> FiberM
liftAffToFiberM = lift <<< lift

delayRandom :: Aff Number
delayRandom = delay (Milliseconds 1000.0) *> randomAff