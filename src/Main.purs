module Main where

import Prelude

import Parser as Parser
import Effect (Effect)

main :: Effect Unit
main = Parser.test
