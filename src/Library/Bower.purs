module Library.Bower
  ( VersionList
  , getVersionList
  , getDependencies
  ) where

import Prelude

import Data.Array (catMaybes, filter, snoc)
import Data.List.NonEmpty (NonEmptyList, fromFoldable)
import Data.Maybe (maybe)
import Data.String.Utils (startsWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, catchError, error, throwError)
import Foreign (Foreign, readArray, readString)
import Foreign.Index ((!))
import Foreign.Keys (keys)
import Library.Helper (eitherToAff, fToAff, parseJSON)
import Library.Type (Dependency, LibraryName)
import Library.Type.Version (validVersionRange)
import Node.Execa (execa)
import Node.Semver (Version, valid)


type VersionList = NonEmptyList Version


getVersionList ∷ LibraryName → Aff VersionList
getVersionList
    = execBowerInfo
  >=> getVersionListFromJson


getVersionListFromJson ∷ Foreign → Aff VersionList
getVersionListFromJson =
  (     (_ ! "versions")
    >=> readArray
    >=> traverse readString
  ) >>> map (fromFoldable <<< catMaybes <<< map valid)
    >>> fToAff
    >=> maybe (throwError $ error "No version") pure


getDependencies ∷ LibraryName → Version → Aff (Array Dependency)
getDependencies name version =
  let target = name <> "@" <> show version
   in getDependenciesFromJSON =<< execBowerInfo target


getDependenciesFromJSON ∷ Foreign → Aff (Array Dependency)
getDependenciesFromJSON json =
  flip catchError (const $ pure []) $ fToAff do
    dependencies ← json ! "dependencies"
    names ← filter (startsWith "purescript-") <$> keys dependencies
    traverse (getVersionStr dependencies) names

  where
    getVersionStr dependencies name
        = (Tuple name <<< validVersionRange)
      <$> (readString =<< (dependencies ! name))


execBowerInfo ∷ String → Aff Foreign
execBowerInfo
    = snoc ["info", "-j"]
  >>> execa "bower"
  >=> parseJSON
  >>> eitherToAff
