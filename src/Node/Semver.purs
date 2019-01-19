module Node.Semver
  ( Version
  , valid
  , Range
  , validRange
  , satisfies
  , maxSatisfying
  ) where


import Prelude

import Data.Function (on)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Unsafe.Coerce (unsafeCoerce)

--

foreign import data Version ∷ Type

instance showVersion ∷ Show Version where
  show = unsafeCoerce

instance eqVersion ∷ Eq Version where
  eq = runFn2 _eq

instance ordVersion ∷ Ord Version where
  compare a b
    | a == b = EQ
    | a `runFn2 _gt` b = GT
    | otherwise = LT


valid ∷ String → Maybe Version
valid = toMaybe <<< runFn1 _valid

--

foreign import data Range ∷ Type

instance showRange ∷ Show Range where
  show = unsafeCoerce

instance eqRange ∷ Eq Range where
  eq = eq `on` show

validRange ∷ String → Maybe Range
validRange = toMaybe <<< runFn1 _validRange

satisfies ∷ Version → Range → Boolean
satisfies = runFn2 _satisfies

maxSatisfying ∷ Array Version → Range → Maybe Version
maxSatisfying = (toMaybe <<< _) <<< runFn2 _maxSatisfying

--

foreign import _valid ∷ Fn1 String (Nullable Version)

foreign import _eq ∷ Fn2 Version Version Boolean
foreign import _gt ∷ Fn2 Version Version Boolean

foreign import _validRange ∷ Fn1 String (Nullable Range)
foreign import _satisfies ∷ Fn2 Version Range Boolean
foreign import _maxSatisfying ∷ Fn2 (Array Version) Range (Nullable Version)
