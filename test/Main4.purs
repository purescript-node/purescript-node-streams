-- | How to test:
-- |
-- | ```
-- | pulp test --main Test.Main4
-- | ```
-- |
-- | This is a test that `readSome` will prevent the Node.js event
-- | loop from exiting.
module Test.Main4 where

import Prelude

import Control.Alt (alt)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Error, Milliseconds(..), delay, parallel, runAff_, sequential)
import Effect.Class.Console as Console
import Node.Stream (Readable)
import Node.Stream.Aff (readSome)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

foreign import stdin :: Readable ()

completion :: Either Error (Effect Unit) -> Effect Unit
completion = case _ of
  Left e -> Console.error (unsafeCoerce e)
  Right f -> f

main :: Effect Unit
main = unsafePartial $ do
  runAff_ completion do
    runSpec [ consoleReporter ] do
      describe "Node.Stream.Aff" do
        it "reads 1" do
          sequential $ alt
            do
              parallel $ void $ readSome stdin
            do
              parallel $ delay (Milliseconds 500.0)
    pure (pure unit)
