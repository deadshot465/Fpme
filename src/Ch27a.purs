module Ch27a where
  
import Prelude

import ChalkStyles (Style, bold, dim, red, strikethrough)
import Data.Array ((:))
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Class.Console (log)

foreign import _chalk :: Fn2 (Array Style) String String

test :: Effect Unit
test = do
  let colorful styles = chalk $ red : styles
  log $ colorful [ bold, strikethrough ] "Test"
    <> colorful [] "Test" <> colorful [ dim ] "Test"

chalk :: Array Style -> String -> String
chalk styles str = runFn2 _chalk styles str