module Library.Type
  ( Account
  , LibraryName
  , URL
  , Body
  , Dependency
  , DependencyList(..)
  ) where

import Prelude

import Data.Tuple (Tuple)
import Library.Type.Version (VersionRange)


type Account = String
type LibraryName = String
type URL = String
type Body = String

type Dependency = Tuple LibraryName VersionRange
data DependencyList
  = Broken
  | DependencyList (Array Dependency)

instance showDependencyList ∷ Show DependencyList where
  show Broken = "broken"
  show (DependencyList list) = show list

instance eqDependencyList ∷ Eq DependencyList where
  eq Broken Broken = true
  eq Broken (DependencyList _) = false
  eq (DependencyList _) Broken = false
  eq (DependencyList l1) (DependencyList l2) = l1 == l2
