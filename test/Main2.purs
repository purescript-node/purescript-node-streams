-- | How to test:
-- |
-- | ```
-- | pulp test --main Test.Main2
-- | ```
module Test.Main2 where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Error, error, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Stream (newPassThrough)
import Node.Stream.Aff (readableToStringUtf8, write)
import Unsafe.Coerce (unsafeCoerce)

completion :: Either Error Unit -> Effect Unit
completion = case _ of
  Left e -> Console.error (unsafeCoerce e)
  Right _ -> mempty

main :: Effect Unit
main = do
  duplex <- newPassThrough
  runAff_ completion do
    let expected = 100_000
    b <- liftEffect $ Buffer.fromString "aaaaaaaaaa" UTF8
    write duplex $ Array.replicate 100000 b
    str <- readableToStringUtf8 duplex
    let actual = Array.length (String.split (Pattern "\n") str)
    unless (expected == expected) do
      throwError $ error $ "Expected " <> show expected <> " lines, but got " <> show actual <> " lines."

