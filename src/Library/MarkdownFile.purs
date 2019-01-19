module Library.MarkdownFile
  ( writeMarkdownFiles
  , makeContentAll
  , makeContent012
  ) where

import Prelude

import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap)
import Data.Foldable (for_, intercalate)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.JSDate (now, toUTCString)
import Data.Map (filter)
import Data.Maybe (maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, uncurry)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Library (LibraryMap)
import Library.PreludeVersionMap as PM
import Library.Type (DependencyList(..))
import Library.Type.Version (makeVersion)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Semver (Version)


dir ∷ String
dir = "./doc/"

files ∷ Array (Tuple String (LibraryMap → Effect String))
files =
  [ Tuple "library-list-all.md" makeContentAll
  , Tuple "library-list-compatible.md" makeContent012
  ]


writeMarkdownFiles ∷ LibraryMap → Aff Unit
writeMarkdownFiles libraryMap
    = for_ files $
            bimap (dir <> _) (_ $ libraryMap)
        >>> sequence
        >>> liftEffect
        >=> uncurry (writeTextFile UTF8)


makeContentAll ∷ LibraryMap → Effect String
makeContentAll libraryMap = do
  dateTime ← toUTCString <$> now
  pure $ header dateTime
         <> foldMapWithIndex makeLibraryContent libraryMap

  where
    header dateTime
       = "[back](../README.md)\n\n"
      <> "# Purescript Library List\n\n"
      <> "update: " <> dateTime <> "\n\n"
      <> "| library | version | 0.12 compatibility | Pursuit |\n"
      <> "| :--- | :---: | :---: | :---: |\n"

    makeLibraryContent name libraryInfo =
          "| " <> name
      <> " | " <> show libraryInfo.version
      <> " | " <> compatibilityStr libraryInfo.preludeVersion
      <> " | " <> maybe "" pursuitStr libraryInfo.pursuit
      <> " |\n"

    pursuitStr info = "[" <> show info.version <> "]"
                   <> "(" <> info.url <> ")"

    compatibilityStr PM.Unknown = "?"
    compatibilityStr (PM.Prelude version) | version >= targetVersion = "✔"
    compatibilityStr _ = ""


makeContent012 ∷ LibraryMap → Effect String
makeContent012 libraryMap = do
  dateTime ← toUTCString <$> now
  let compatibleLibraryMap = filter isCompatible libraryMap
  pure $ header dateTime
         <> foldMapWithIndex makeLibraryContent compatibleLibraryMap

  where
    header dateTime
       = "[back](../README.md)\n\n"
      <> "# Purescript Library List (compatible with 0.12)\n\n"
      <> "update: " <> dateTime <> "\n\n"
      <> "| library | version | dependency | Pursuit |\n"
      <> "| :--- | :---: | :--- | :---: |\n"

    isCompatible {preludeVersion: PM.Prelude version} = version >= targetVersion
    isCompatible _ = false

    makeLibraryContent name libraryInfo =
          "| " <> name
      <> " | " <> show libraryInfo.version
      <> " | " <> dependenciesStr libraryInfo.dependency
      <> " | " <> maybe "" pursuitStr libraryInfo.pursuit
      <> " |\n"

    pursuitStr info = "[" <> show info.version <> "]"
                   <> "(" <> info.url <> ")"

    dependenciesStr (DependencyList list)
      = intercalate "<br>" $ dependencyStr <$> list
    dependenciesStr _ = show Broken

    dependencyStr = bifoldMap (_ <> " ") fst


targetVersion ∷ Version
targetVersion = makeVersion 4 0 0
