module Ch23a where
  
import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)

test :: Effect Unit
test = launchAff_ do
  ttAVar <- AVar.empty
  clockFiber <- forkAff $ clock ttAVar
  bombFiber <- forkAff $ bomb ttAVar 3
  AVar.put Tick ttAVar
  joinFiber bombFiber
  killFiber (error "Exploded!") clockFiber

data TickTock = Tick | Tock
derive instance eqTickTock :: Eq TickTock

data BombState = WaitingTick | WaitingTock

clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
  void $ AVar.take ttAVar
  delay $ Milliseconds 1000.0
  AVar.put Tock ttAVar
  void $ AVar.take ttAVar
  delay $ Milliseconds 1000.0
  AVar.put Tick ttAVar
  clock ttAVar

bomb :: AVar TickTock -> Int -> Aff Unit
bomb ttAVar destinationCount = go 0 WaitingTick
  where
    go count state = do
      if count == destinationCount then log "BOOM!!"
      else do
        delay $ Milliseconds 500.0
        tt <- AVar.read ttAVar
        case state of
          WaitingTick ->
            if tt == Tick then log "Tick" *> go count WaitingTock
            else go count state
          WaitingTock ->
            if tt == Tock then log "Tock" *> go (count + 1) WaitingTick
            else go count state