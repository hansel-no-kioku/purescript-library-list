module Test.Main where

import Prelude

import Data.Array (length)
import Data.List.NonEmpty as LN
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (error, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Library (Status)
import Library as L
import Library.Bower as B
import Library.Helper (parseToAff)
import Library.MarkdownFile (makeContent012, makeContentAll, writeMarkdownFiles)
import Library.Pursuit as P
import Library.Type.Version (makeVersion, parserLatestVersion, parserVersion, validVersionRange)
import Node.Puppeteer (URL(..), browse, goto, tryGetContent)
import Node.Semver (maxSatisfying, satisfies, valid, validRange)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Assertions.Aff (expectError)
import Test.Spec.Assertions.String (shouldContain)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, run')
import Text.Parsing.StringParser (runParser)


main :: Effect Unit
main = run' testConfig [consoleReporter] do
  testPuppeteer
  testSemver
  testVersion
  testLibraryPursuit
  testLibraryBower
  testLibrary
  testMarkdown
  testAll

  where
    testConfig = defaultConfig { timeout = Nothing }


testPuppeteer ∷ Spec Unit
testPuppeteer = describe "Puppeteer" do

  it "Browse" do
    browse \page → pure unit

  describe "Try get content" do

    it "Success" do
      browse \page → do
        goto (URL "https://pursuit.purescript.org/") page
        body ← tryGetContent page 1 pure
        body `shouldContain` "https://pursuit.purescript.org/packages/purescript-spec"

    it "Fail" do
      browse \page → do
        goto (URL "https://pursuit.purescript.org/") page
        expectError $ tryGetContent page 1 \_ → throwError $ error "Fail"


testSemver ∷ Spec Unit
testSemver = describe "Semver" do

  describe "Version" do

    it "valid 1.2.3" do
      (show <$> valid "v1.2.3") `shouldEqual` Just "1.2.3"

    it "valid x.x.x" do
      valid "x.x.x" `shouldEqual` Nothing

    it "eq" do
      valid "1.2.3" `shouldEqual` valid "v1.2.3"

    it "Compare major" do
      valid "4.1.2" `shouldSatisfy` (_ > valid "3.2.2")

    it "Compare minor" do
      valid "3.1.3" `shouldSatisfy` (_ < valid "3.2.2")

  describe "Range" do

    it "valid ^1.2.3" do
      (show <$> validRange "^1.2.3") `shouldEqual` Just ">=1.2.3 <2.0.0"

    it "valid ^1.x" do
      (show <$> validRange "^1.x") `shouldEqual` Just ">=1.0.0 <2.0.0"

    it "1.3.0 satisfies ^1.2.3" do
      (satisfies <$> valid "1.3.0" <*> validRange "^1.2.3")
        `shouldEqual` Just true

    it "maxSatisfying ^1.2.3" do
      let versions = traverse valid [ "2.0.0", "1.1.0", "1.0.0", "0.9.0" ]
      join (maxSatisfying <$> versions <*> validRange "^1.0.0")
        `shouldEqual` valid "1.1.0"


testVersion ∷ Spec Unit
testVersion = describe "Version" do

  describe "Parse" do

    it "Parse" do
      result ← parseToAff $ runParser parserVersion "3.2.10"
      show result `shouldEqual` "3.2.10"

    it "Parse error" do
      expectError $ parseToAff $ runParser parserVersion "x.x.x"

    it "Parse latest version" do
      result ← parseToAff $ runParser parserLatestVersion "latest (7.8.9)"
      show result `shouldEqual` "7.8.9"


testLibraryPursuit ∷ Spec Unit
testLibraryPursuit = describe "Library.Pursuit" do

  it "Get libraries" do
    libs ← browse P.getLibraries
    length libs `shouldSatisfy` (_ > 0)

  it "Get Pursuit info" do
    let url = "https://pursuit.purescript.org/packages/purescript-conditional"
        version = makeVersion 2 0 0
        info = { url, version }
    result ← browse \page → P.getPursuitInfo page url
    result `shouldEqual` info


testLibraryBower ∷ Spec Unit
testLibraryBower = describe "Library.Bower" do

  it "Get versions" do
    result ← B.getVersionList "purescript-effect"
    LN.length result `shouldSatisfy` (_ >= 4)

  it "Get dependencies" do
    let dependency =
          [ Tuple "purescript-prelude" $ validVersionRange "^4.0.0"
          ]
    result ← B.getDependencies "purescript-effect" $ makeVersion 2 0 0
    result `shouldEqual` dependency


testLibrary ∷ Spec Unit
testLibrary = describe "Library" do

  describe "Get libraries" do

    it "Success" do
      let status = (mempty ∷ Status)
            { targetList = [ Tuple "purescript-aff" $ makeVersion 5 1 0 ] }
      lib ← browse $ flip L.updateLibraryMap status
      logShow lib
      pure unit

    it "Broken" do
      let status = (mempty ∷ Status)
            { targetList = [ Tuple "purescript-big-integer" $ makeVersion 0 0 1 ] }
      lib ← browse $ flip L.updateLibraryMap status
      logShow lib
      pure unit


testMarkdown ∷ Spec Unit
testMarkdown = describe "Library.MarkdownFile" do

  it "All" do
    let status = (mempty ∷ Status)
          { targetList =
            [ Tuple "purescript-aff" $ makeVersion 5 1 0
            , Tuple "purescript-conditional" $ makeVersion 2 0 0
            ]
          }
    lib ← browse $ flip L.updateLibraryMap status
    content ← liftEffect $ makeContentAll lib
    log content
    pure unit

  it "0.12 compatible" do
    let status = (mempty ∷ Status)
          { targetList =
            [ Tuple "purescript-aff" $ makeVersion 5 1 0
            , Tuple "purescript-conditional" $ makeVersion 2 0 0
            ]
          }
    lib ← browse $ flip L.updateLibraryMap status
    content ← liftEffect $ makeContent012 lib
    log content
    pure unit

  -- it "Write markdown files" do
  --   let status = (mempty ∷ Status)
  --         { targetList =
  --           [ Tuple "purescript-aff" $ makeVersion 5 1 0
  --           , Tuple "purescript-conditional" $ makeVersion 2 0 0
  --           ]
  --         }
  --   lib ← browse $ flip L.updateLibraryMap status
  --   writeMarkdownFiles lib


testAll ∷ Spec Unit
testAll = describe "All" do

  it "Get libraries" do
    lib ← L.getLibraries
    logShow lib


bowerInfoPurescriptEffect ∷ String
bowerInfoPurescriptEffect = """

bower purescript-effect#*       cached https://github.com/purescript/purescript-effect.git#2.0.0
bower purescript-effect#*     validate 2.0.0 against https://github.com/purescript/purescript-effect.git#*

{
  name: 'purescript-effect',
  homepage: 'https://github.com/purescript/purescript-effect',
  license: 'BSD-3-Clause',
  repository: {
    type: 'git',
    url: 'git://github.com/purescript/purescript-effect.git'
  },
  ignore: [
    '**/.*',
    'bower_components',
    'node_modules',
    'output',
    'test',
    'bower.json',
    'package.json'
  ],
  dependencies: {
    'purescript-prelude': '^4.0.0'
  },
  version: '2.0.0'
}

Available versions:
  - 2.0.0
  - 1.1.0
  - 1.0.0
  - 0.1.0

"""
