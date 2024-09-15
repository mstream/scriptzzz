module Scriptzzz.Sandbox (execJs) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import execJsImpl :: Fn2 Int String (Effect (Promise String))

execJs :: Int -> String -> Aff String
execJs timeout code = toAffE $ runFn2 execJsImpl timeout code

