module Library.DependencyMap
  ( DependencyMap
  , insert
  , lookup
  ) where

import Prelude

import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Library.Type (DependencyList, LibraryName)
import Node.Semver (Version)

type DependencyMap = M.Map LibraryName (M.Map Version DependencyList)


insert
   ∷ LibraryName
  → Version
  → DependencyList
  → DependencyMap
  → DependencyMap
insert name version dependencyList dependencyMap =
  let info = fromMaybe mempty $ M.lookup name dependencyMap
      newInfo = M.insert version dependencyList info
   in M.insert name newInfo dependencyMap


lookup ∷ LibraryName → Version → DependencyMap → Maybe DependencyList
lookup name version dependencyMap = do
  info ← M.lookup name dependencyMap
  M.lookup version info
