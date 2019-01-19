module Node.Puppeteer
  ( module Toppokki
  , browse
  , tryGetContent
  ) where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), bracket, catchError, delay, throwError)
import Toppokki (Browser, Page, URL(..), WaitUntilOption, close, content, goto, launch, newPage, waitForNavigation)


browse ∷ ∀ a. (Page → Aff a) → Aff a
browse f = bracket (launch {}) close $ f <=< newPage


tryGetContent ∷ ∀ a. Page → Int → (String → Aff a) → Aff a
tryGetContent page retryCount func = do
  content ← content page
  catchError (func content)
             \e → if retryCount <= 1
                then throwError e
                else do
                  delay (Milliseconds 200.0)
                  tryGetContent page (retryCount - 1) func
