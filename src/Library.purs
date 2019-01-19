module Library
  ( LibraryInfo
  , LibraryMap
  , PursuitMap
  , VersionMap
  , Target
  , Status
  , getLibraries
  , updateLibraryMap
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, evalStateT, gets, lift, modify_)
import Data.Array (elemIndex, snoc, uncons)
import Data.Foldable (foldMap)
import Data.List (List)
import Data.List.NonEmpty as LN
import Data.Map (Map, fromFoldable, insert, lookup, singleton, toUnfoldable)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, uncurry)
import Effect.Aff (Aff, catchError)
import Effect.Class.Console (log, logShow)
import Library.Bower as B
import Library.DependencyMap as DM
import Library.PreludeVersionMap as PM
import Library.Pursuit as P
import Library.Type (Dependency, DependencyList(..), LibraryName)
import Library.Type.Version (maxSatisfyingVersion)
import Node.Puppeteer (Page, browse)
import Node.Semver (Version)


type PursuitMap = Map LibraryName P.PursuitInfo
type VersionMap = Map LibraryName B.VersionList

type LatestVersionInfo =
  { version ∷ Version
  , dependency ∷ DependencyList
  , preludeVersion ∷ PM.PreludeVersion
  }
type LatestVersionMap = Map LibraryName LatestVersionInfo

type LibraryInfo =
  { version ∷ Version
  , dependency ∷ DependencyList
  , preludeVersion ∷ PM.PreludeVersion
  , pursuit ∷ Maybe P.PursuitInfo
  }

type LibraryMap = Map LibraryName LibraryInfo

type Target = Tuple LibraryName Version

targetToStr ∷ Tuple LibraryName Version → String
targetToStr (Tuple name version) = name <> "@" <> show version

type Breadcrumb = Array Target

type Status =
  { pursuitMap ∷ PursuitMap
  , versionMap ∷ VersionMap
  , dependencyMap ∷ DM.DependencyMap
  , preludeVersionMap ∷ PM.PreludeVersionMap
  , targetList ∷ Array Target
  }

type S = StateT Status Aff


getLibraries ∷ Aff LibraryMap
getLibraries = browse \page → do
  pursuitMap ← getPursuitMap page
  versionMap ← getVersionMap $ keys pursuitMap
  let status = initialStatus pursuitMap versionMap
  updateLibraryMap page status


getPursuitMap ∷ Page → Aff PursuitMap
getPursuitMap page = do
  log "Get libraries from Pursuit"
  libraries ← P.getLibraries page
  fromFoldable <$> traverse getPursuitInfo' libraries

  where
    getPursuitInfo' (Tuple name url) = do
      log $ "Get info: " <> name <> " <- " <> url
      Tuple name <$> P.getPursuitInfo page url


getVersionMap ∷ List LibraryName → Aff VersionMap
getVersionMap list = foldMap getBersionInfo list
  where
    getBersionInfo name = do
      log $ "Get versions: " <> name
      catchError
        (singleton name <$> B.getVersionList name)
        ((mempty <$ _) <<< logShow)


initialStatus ∷ PursuitMap → VersionMap → Status
initialStatus pursuitMap versionMap =
  { pursuitMap
  , versionMap
  , dependencyMap: mempty
  , preludeVersionMap: mempty
  , targetList: makeTargetList versionMap
  }

  where
    makeTargetList ∷ VersionMap → Array Target
    makeTargetList = map (map LN.head) <<< toUnfoldable


updateLibraryMap ∷ Page → Status → Aff LibraryMap
updateLibraryMap page =
  evalStateT (tailRecM update mempty)

  where
    update libraryMap = do
      t ← getTarget
      case t of
        Just target → do
          libraryInfo ← getLibraryInfo page target
          pure $ Loop $ insert (fst target) libraryInfo libraryMap
        _ → pure $ Done libraryMap


getTarget ∷ S (Maybe Target)
getTarget =
      gets _.targetList
  >>= uncons
  >>> maybe (pure Nothing)
            (\{head, tail} → Just head <$ modify_ _{targetList = tail})


getLibraryInfo ∷ Page → Target → S LibraryInfo
getLibraryInfo page target@(Tuple name version) = do
  log $ "Get library info: " <> targetToStr target
  { version: version
  , dependency: _
  , preludeVersion: _
  , pursuit: _
  }
    <$> getDependencyList target
    <*> getPreludeVersion page [] target
    <*> getPursuitInfo name


getDependencyList ∷ Target → S DependencyList
getDependencyList target =
      gets _.dependencyMap
  >>= uncurry DM.lookup target
  >>> maybe (updateDependencyMap target) pure


updateDependencyMap ∷ Target → S DependencyList
updateDependencyMap target = do
  log $ "  Get dependency list: " <> targetToStr target
  dependencyList ← getDependencies
  modify_ \status@{dependencyMap} →
    let newDependencyMap = uncurry DM.insert target dependencyList dependencyMap
    in status{dependencyMap = newDependencyMap}
  pure dependencyList

  where
    getDependencies = do
      catchError
        (lift $ DependencyList <$> uncurry B.getDependencies target)
        (\e → Broken <$ logShow e)


getVersionList ∷ LibraryName → S B.VersionList
getVersionList name =
      gets _.versionMap
  >>= lookup name
  >>> maybe (updateVersionMap name) pure


updateVersionMap ∷ LibraryName → S B.VersionList
updateVersionMap name = do
  log $ "  Get versions: " <> name
  versionList ← lift $ B.getVersionList name
  modify_ \status@{versionMap, targetList} →
    let newVersionMap = insert name versionList versionMap
        newTarget = Tuple name $ LN.head versionList
        newTargetList = targetList `snoc` newTarget
      in status {versionMap = newVersionMap, targetList = newTargetList}
  pure versionList


getPreludeVersion ∷ Page → Breadcrumb → Target → S PM.PreludeVersion
getPreludeVersion page breadcrumb target@(Tuple name version)
  | name == "purescript-prelude" = pure $ PM.Prelude version
  | Just preludeVersion ← PM.lookupOfficials name version = pure preludeVersion
  | Just _ ← elemIndex target breadcrumb = pure PM.Unknown
  | otherwise =
          gets _.preludeVersionMap
      >>= uncurry PM.lookup target
      >>> maybe (updatePreludeVersionMap page breadcrumb target) pure


updatePreludeVersionMap ∷ Page → Breadcrumb → Target → S PM.PreludeVersion
updatePreludeVersionMap page breadcrumb target = do
  dependencyList ← getDependencyList target
  case dependencyList of
    DependencyList list → do
      preludeVersion ← getDependentPreludeVersion page
                                                   (snoc breadcrumb target)
                                                   PM.Unknown list
      modify_ $ updatePreludeVersionMap' preludeVersion
      pure preludeVersion
    _ → pure PM.Unknown

  where
    updatePreludeVersionMap' ∷ PM.PreludeVersion → Status → Status
    updatePreludeVersionMap' preludeVersion status@{preludeVersionMap} =
      let newMap = uncurry PM.insert target preludeVersion preludeVersionMap
       in status {preludeVersionMap = newMap}


getDependentPreludeVersion
   ∷ Page
  → Breadcrumb
  → PM.PreludeVersion
  → Array Dependency
  → S PM.PreludeVersion
getDependentPreludeVersion page breadcrumb now list =
  case uncons list of
    Just { head, tail } → do
      preludeVersion ← getPreludeVersion' head
      case preludeVersion of
        PM.Prelude _ → pure preludeVersion
        _ → getDependentPreludeVersion page breadcrumb now tail
    _ → pure now

  where
    getPreludeVersion' (Tuple name range) = do
      versionList ← getVersionList name
      let versions = LN.toUnfoldable versionList
      case maxSatisfyingVersion versions range of
        Just version → getPreludeVersion page breadcrumb $ Tuple name version
        _ → pure PM.Unknown


getPursuitInfo ∷ LibraryName → S (Maybe P.PursuitInfo)
getPursuitInfo name = lookup name <$> gets _.pursuitMap
