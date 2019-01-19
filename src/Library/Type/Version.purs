module Library.Type.Version
  ( parserVersion
  , parserLatestVersion
  , makeVersion
  , VersionRange
  , validVersionRange
  , satisfyVersion
  , maxSatisfyingVersion
  ) where

import Prelude hiding (between)

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Node.Semver (Range, Version, maxSatisfying, satisfies, validRange)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, regex, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (between)
import Unsafe.Coerce (unsafeCoerce)


parserVersion ∷ Parser Version
parserVersion = unsafeCoerce <$> regex "\\d+\\.\\d+\\.\\d+"

parserLatestVersion ∷ Parser Version
parserLatestVersion = do
  skipSpaces
  _ ← string "latest"
  skipSpaces
  between (char '(') (char ')')
    (skipSpaces *> parserVersion <* skipSpaces)

makeVersion ∷ Int → Int → Int → Version
makeVersion major minor patch =
  unsafeCoerce $ intercalate "." $  show <$> [major, minor, patch]


type VersionRange = Tuple String (Maybe Range)

validVersionRange ∷ String → VersionRange
validVersionRange rangeStr =
  let range = validRange rangeStr
   in Tuple (if isJust range then rangeStr else "") range

satisfyVersion ∷ Version → VersionRange → Boolean
satisfyVersion version (Tuple _ (Just range)) = satisfies version range
satisfyVersion _ _ = false

maxSatisfyingVersion ∷ Array Version → VersionRange → Maybe Version
maxSatisfyingVersion list (Tuple _ (Just range)) = maxSatisfying list range
maxSatisfyingVersion _ _ = Nothing
