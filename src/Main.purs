module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Library (getLibraries)
import Library.MarkdownFile (writeMarkdownFiles)

main :: Effect Unit
main = launchAff_ $
  writeMarkdownFiles =<<  getLibraries
