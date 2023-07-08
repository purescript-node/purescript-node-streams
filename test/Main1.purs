-- | How to test:
-- |
-- | ```
-- | pulp test --main Test.Main1
-- | ```
-- |
-- | We want to read from a file, not stdin, because stdin has no EOF.
module Test.Main1 where

import Prelude

import Control.Parallel (parSequence_)
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer, concat)
import Node.Buffer as Buffer
import Node.Stream (Readable, Writable, destroy, newPassThrough)
import Node.Stream as Stream
import Node.Stream.Aff (end, fromStringUTF8, readAll, readN, readSome, toStringUTF8, write)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

foreign import createReadStream :: String -> Effect (Readable ())
foreign import createWriteStream :: String -> Effect (Writable ())

main :: Effect Unit
main = unsafePartial $ do
  launchAff_ do
    runSpec' (defaultConfig { timeout = Just (Milliseconds 40000.0) }) [ consoleReporter ] do
      describe "Node.Stream.Aff" do
        it "PassThrough" do
          s <- liftEffect $ newPassThrough
          _ <- write s =<< fromStringUTF8 "test"
          end s
          b1 <- toStringUTF8 =<< readAll s
          shouldEqual b1 "test"
        it "overflow PassThrough" do
          s <- liftEffect $ newPassThrough
          let magnitude = 10000
          [ outstring ] <- fromStringUTF8 "aaaaaaaaaa"
          parSequence_
            [ write s $ Array.replicate magnitude outstring
            , void $ readSome s
            ]
        it "reads from a zero-length Readable" do
          r <- liftEffect $ Stream.fromString ""
          -- readSome should return readagain false
          shouldEqual { buffers: "", readagain: true } =<< toStringBuffers =<< readSome r
          shouldEqual "" =<< toStringUTF8 =<< readAll r
          shouldEqual { buffers: "", readagain: false } =<< toStringBuffers =<< readN r 0
        it "readN cleans up event handlers" do
          s <- liftEffect $ Stream.fromString ""
          for_ (0 .. 100) \_ -> void $ readN s 0
        it "readSome cleans up event handlers" do
          s <- liftEffect $ Stream.fromString ""
          for_ (0 .. 100) \_ -> void $ readSome s
        it "readAll cleans up event handlers" do
          s <- liftEffect $ Stream.fromString ""
          for_ (0 .. 100) \_ -> void $ readAll s
        it "write cleans up event handlers" do
          s <- liftEffect $ newPassThrough
          [ b ] <- liftEffect $ fromStringUTF8 "x"
          for_ (0 .. 100) \_ -> void $ write s [ b ]
        it "readSome from PassThrough" do
          s <- liftEffect $ newPassThrough
          write s =<< fromStringUTF8 "test"
          end s
          -- The first readSome readagain will be true, that's not good
          shouldEqual { buffers: "test", readagain: true } =<< toStringBuffers =<< readSome s
          shouldEqual { buffers: "", readagain: false } =<< toStringBuffers =<< readSome s
        it "readSome from PassThrough concurrent" do
          s <- liftEffect $ newPassThrough
          parSequence_
            [ do
                shouldEqual { buffers: "test", readagain: true } =<< toStringBuffers =<< readSome s
                -- This is rediculous behavior
                shouldEqual { buffers: "", readagain: true } =<< toStringBuffers =<< readSome s
                shouldEqual { buffers: "", readagain: false } =<< toStringBuffers =<< readSome s
            , do
                write s =<< fromStringUTF8 "test"
                end s
            ]
        it "readAll from PassThrough concurrent" do
          s <- liftEffect $ newPassThrough
          parSequence_
            [ do
                shouldEqual "test" =<< toStringUTF8 =<< readAll s
            , do
                write s =<< fromStringUTF8 "test"
                end s
            ]
        it "readAll from empty PassThrough concurrent" do
          s <- liftEffect $ newPassThrough
          parSequence_
            [ shouldEqual "" =<< toStringUTF8 =<< readAll s
            , end s
            ]
        it "readSome from destroyed PassThrough" do
          s <- liftEffect $ newPassThrough
          liftEffect $ destroy s
          shouldEqual { buffers: "", readagain: false } =<< toStringBuffers =<< readSome s
        it "readSome from destroyed PassThrough concurrent" do
          s <- liftEffect $ newPassThrough
          parSequence_
            [ shouldEqual { buffers: "", readagain: false } =<< toStringBuffers =<< readSome s
            , liftEffect $ destroy s
            ]
        it "readAll from destroyed PassThrough concurrent " do
          s <- liftEffect $ newPassThrough
          parSequence_
            [ shouldEqual "" =<< toStringUTF8 =<< readAll s
            , liftEffect $ destroy s
            ]
        it "readN from destroyed PassThrough concurrent " do
          s <- liftEffect $ newPassThrough
          parSequence_
            [ shouldEqual { buffers: "", readagain: false } =<< toStringBuffers =<< readN s 1
            , liftEffect $ destroy s
            ]
        it "write to destroyed PassThrough" do
          s <- liftEffect $ newPassThrough
          liftEffect $ destroy s
          expectError $ write s =<< fromStringUTF8 "test"
        it "writes and reads to file" do
          let outfilename = "/tmp/test1.txt"
          let magnitude = 100000
          outfile <- liftEffect $ createWriteStream outfilename
          [ outstring ] <- fromStringUTF8 "aaaaaaaaaa"
          write outfile $ Array.replicate magnitude outstring
          infile <- liftEffect $ createReadStream outfilename
          { buffers: input1 } <- readSome infile
          { buffers: input2 } <- readN infile (5 * magnitude)
          input3 <- readAll infile
          _ :: Buffer <- liftEffect <<< concat <<< _.buffers =<< readSome infile
          void $ readN infile 1
          void $ readAll infile
          let inputs = input1 <> input2 <> input3
          input :: Buffer <- liftEffect $ concat inputs
          inputSize <- liftEffect $ Buffer.size input
          shouldEqual inputSize (10 * magnitude)
        it "writes and closes file" do
          let outfilename = "/tmp/test2.txt"
          outfile <- liftEffect $ createWriteStream outfilename
          write outfile =<< fromStringUTF8 "test"
          end outfile
          expectError $ write outfile =<< fromStringUTF8 "test2"

    pure unit

toStringBuffers
  :: { buffers :: Array Buffer, readagain :: Boolean }
  -> Aff { buffers :: String, readagain :: Boolean }
toStringBuffers { buffers, readagain } = do
  buffers' <- toStringUTF8 buffers
  pure { buffers: buffers', readagain }
