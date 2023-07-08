-- | How to test:
-- |
-- | ```
-- | spago -x spago-dev.dhall test --main Test2 | wc -c
-- | ```
module Test2 where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Process (stdout)
import Node.Stream.Aff (write)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

completion :: Either Error (Effect Unit) -> Effect Unit
completion = case _ of
  Left e -> Console.error (unsafeCoerce e)
  Right f -> f

main :: Effect Unit
main = unsafePartial $ do
  runAff_ completion do
    do
      b <- liftEffect $ Buffer.fromString "aaaaaaaaaa" UTF8
      write stdout $ Array.replicate 100000 b
    pure (pure unit)
