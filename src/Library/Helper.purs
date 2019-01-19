module Library.Helper
  ( eitherToAff
  , eitherToF
  , fToEither
  , fToAff
  , parseToEither
  , parseToAff
  , parseJSON
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Function.Uncurried (Fn3, runFn3)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error, error, message)
import Foreign (F, Foreign, ForeignError(..), fail, renderForeignError)
import Text.Parsing.StringParser (ParseError)


eitherToAff ∷ Either Error ~> Aff
eitherToAff e = makeAff \cb → cb e $> nonCanceler


eitherToF ∷ Either Error ~> F
eitherToF = either (fail <<< ForeignError <<< message) pure


fToEither ∷ F ~> Either Error
fToEither = bimap multipleErrorsToError identity <<< runExcept
  where
    multipleErrorsToError
      = error <<< intercalate "\n" <<< map renderForeignError


fToAff ∷ F ~> Aff
fToAff = fToEither >>> eitherToAff


parseToEither ∷ Either ParseError ~> Either Error
parseToEither = bimap (error <<< show) identity


parseToAff ∷ Either ParseError ~> Aff
parseToAff = eitherToAff <<< parseToEither


parseJSON ∷ String → Either Error Foreign
parseJSON str = runFn3 _parseJSON str Right Left

foreign import _parseJSON ∷ ∀ e a c. Fn3 String (a → c) (e → c) c
