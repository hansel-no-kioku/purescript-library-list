module Library.Pursuit
  ( PursuitInfo
  , getLibraries
  , getPursuitInfo
  ) where

import Prelude

import Cheerio (attr, first, text, toArray)
import Cheerio.Static (load, select)
import Data.Array (catMaybes, filter, nubByEq, sort)
import Data.Function (on)
import Data.String.Utils (startsWith)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Aff)
import Library.Helper (parseToAff)
import Library.Type (LibraryName, URL, Body)
import Library.Type.Version (parserLatestVersion)
import Node.Puppeteer as P
import Node.Semver (Version)
import Text.Parsing.StringParser (runParser)


type PursuitInfo = { url ∷ URL, version ∷ Version }


getLibraries ∷ P.Page → Aff (Array (Tuple LibraryName URL))
getLibraries page = do
  P.goto (P.URL "https://pursuit.purescript.org") page
  getLibraryItems <$> P.content page


getPursuitInfo ∷ P.Page → URL → Aff PursuitInfo
getPursuitInfo page url = do
  P.goto (P.URL url) page
  P.tryGetContent page 10 \body → do
    version ← getVersion body
    pure {url, version }


getLibraryItems ∷ String → Array (Tuple LibraryName URL)
getLibraryItems
    = load
  >>> select "a"
  >>> toArray
  >>> map toLibraryItem
  >>> catMaybes
  >>> filter (fst >>> startsWith "purescript-")
  >>> sort
  >>> nubByEq (eq `on` fst)

  where
    toLibraryItem cheerio = Tuple (text cheerio) <$> attr "href" cheerio


getVersion ∷ Body → Aff Version
getVersion
    = load
  >>> select ".version-selector option"
  >>> first
  >>> text
  >>> runParser parserLatestVersion
  >>> parseToAff
