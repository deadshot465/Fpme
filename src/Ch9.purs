module Ch9 where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Show, Unit, discard, show, ($), (&&), (==))

test :: Effect Unit
test = do
  log $ show $ ATrue <> ATrue
  log $ show $ ATrue <> AFalse
  log $ show $ AFalse <> AFalse
  log $ show $ mempty <> ATrue == ATrue
  log $ show $ mempty <> AFalse == ATrue
  verifyAndBoolSemigroup
  verifyAndBoolMonoid
  verifyOrBoolSemigroup
  verifyOrBoolMonoid

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a

data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append AFalse AFalse = AFalse
  append ATrue AFalse = AFalse
  append AFalse ATrue = AFalse

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "Verifying AndBool Semigroup Laws (1 test)"
  log $ show $ (ATrue <> AFalse) <> ATrue == ATrue <> (AFalse <> ATrue)

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws (2 tests)"
  log $ show $ ATrue <> mempty == mempty <> ATrue && ATrue <> mempty == ATrue
  log $ show $ AFalse <> mempty == mempty <> AFalse && AFalse <> mempty == AFalse

data OrBool = OFalse | OTrue
derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _
instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying OrBool Semigroup Laws (1 test)"
  log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
  log "Verifying OrBool Monoid Laws (2 tests)"
  log $ show $ OTrue <> mempty == mempty <> OTrue && OTrue <> mempty == OTrue
  log $ show $ OFalse <> mempty == mempty <> OFalse && OFalse <> mempty == OFalse