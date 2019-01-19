module Library.PreludeVersionMap
  ( PreludeVersion(..)
  , PreludeVersionMap
  , insert
  , lookup
  , lookupOfficials
  ) where

import Prelude

import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Library.Type (LibraryName)
import Library.Type.Version (makeVersion)
import Node.Semver (Version)


data PreludeVersion
  = Unknown
  | Prelude Version

instance showPreludeVersion ∷ Show PreludeVersion where
  show Unknown = "unknown"
  show (Prelude version) = show version

type PreludeVersionMap = M.Map LibraryName (M.Map Version PreludeVersion)


insert
   ∷ LibraryName
  → Version
  → PreludeVersion
  → PreludeVersionMap
  → PreludeVersionMap
insert name version preludeVersion preludeVersionMap =
  let info = fromMaybe mempty $ M.lookup name preludeVersionMap
   in M.insert name (M.insert version preludeVersion info) preludeVersionMap


lookup ∷ LibraryName → Version → PreludeVersionMap → Maybe PreludeVersion
lookup name version = M.lookup version <=< M.lookup name


lookupOfficials ∷ LibraryName → Version → Maybe PreludeVersion
lookupOfficials name version = M.lookup version =<< M.lookup name officials


officials ∷ PreludeVersionMap
officials = M.fromFoldable
  [ Tuple "purescript-unsafe-coerce" $ M.fromFoldable
      [ mkPreludeVersion 4 0 0    4 0 0
      , mkPreludeVersion 3 0 0    3 0 0
      , mkPreludeVersion 2 0 0    2 1 0
      , mkPreludeVersion 1 0 0    1 0 0
      ]
  , Tuple "purescript-globals" $ M.fromFoldable
      [ mkPreludeVersion 4 0 0    4 0 0
      , mkPreludeVersion 3 0 0    3 0 0
      , mkPreludeVersion 2 0 0    2 1 0
      , mkPreludeVersion 1 0 0    1 0 0
      ]
  , Tuple "purescript-math" $ M.fromFoldable
      [ mkPreludeVersion 2 1 1    4 0 0
      , mkPreludeVersion 2 1 0    3 0 0
      , mkPreludeVersion 2 0 0    2 1 0
      , mkPreludeVersion 1 0 0    0 1 4
      ]
  , Tuple "purescript-partial" $ M.fromFoldable
      [ mkPreludeVersion 2 0 0    4 0 0
      , mkPreludeVersion 1 2 0    2 1 0
      , mkPreludeVersion 1 1 2    0 1 5
      , mkPreludeVersion 1 1 1    0 1 5
      , mkPreludeVersion 1 1 0    0 1 3
      , mkPreludeVersion 1 0 0    0 1 3
      ]
  ]

  where
    mkPreludeVersion mj mn pt pmj pmn ppt
      = Tuple (makeVersion mj mn pt) (Prelude $ makeVersion pmj pmn ppt)
