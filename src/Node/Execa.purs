module Node.Execa
  ( execa
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, makeAff, nonCanceler)
import Effect.Uncurried (EffectFn4, runEffectFn4)


execa ∷ String → Array String → Aff String
execa cmd args = makeAff \cb →
  nonCanceler <$ runEffectFn4 execaImpl cmd args (cb <<< Right) (cb <<< Left)

foreign import execaImpl ∷ EffectFn4
  String
  (Array String)
  (String → Effect Unit)
  (Error → Effect Unit)
  (Effect Unit)
