module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import Effect.Exception as Exception
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Stream (Duplex, Readable, Writable, destroyWithError, end, onData, onDataEither, onDataString, onError, onReadable, passThrough, pipe, pipeline, read, readString, setDefaultEncoding, setEncoding, writeString)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assert')
import Unsafe.Coerce (unsafeCoerce)

assertEqual :: forall a. Show a => Eq a => a -> a -> Effect Unit
assertEqual x y =
  assert' (show x <> " did not equal " <> show y) (x == y)

main :: Effect Boolean
main = do
  log "disabled - setDefaultEncoding should not affect writing"
  -- _ <- testSetDefaultEncoding

  log "setEncoding should not affect reading"
  testSetEncoding

  log "test pipe"
  _ <- testPipe

  log "test pipeline"
  _ <- testPipeline

  log "test write"
  testWrite

  log "test end"
  testEnd

  log "test manual reads"
  testReads

testString :: String
testString = "üöß💡"

testReads :: Effect Boolean
testReads = do
  _ <- testReadString
  testReadBuf

  where
  testReadString = do
    sIn <- passThrough
    v <- readString sIn Nothing UTF8
    assert (isNothing v)

    onReadable sIn do
      str <- readString sIn Nothing UTF8
      assert (isJust str)
      assertEqual (unsafePartial (fromJust str)) testString
      pure unit

    writeString sIn UTF8 testString \_ -> do
      pure unit

  testReadBuf = do
    sIn <- passThrough
    v <- read sIn Nothing
    assert (isNothing v)

    onReadable sIn do
      buf <- read sIn Nothing
      assert (isJust buf)
      _ <- flip assertEqual testString <$> (Buffer.toString UTF8 (unsafePartial (fromJust buf)))
      pure unit

    writeString sIn UTF8 testString \_ -> do
      pure unit

-- testSetDefaultEncoding :: Effect Boolean
-- testSetDefaultEncoding = do
--   w1 <- passThrough
--   _ <- check w1

--   w2 <- passThrough
--   setDefaultEncoding w2 UCS2
--   check w2

--   where
--   check w = do
--     writeString w UTF8 testString \_ -> do
--       c <- getContentsAsString w
--       assertEqual testString c
--     onData

testSetEncoding :: Effect Unit
testSetEncoding = do
  check UTF8
  check UTF16LE
  check UCS2
  where
  check enc = do
    r1 <- passThrough
    _ <- writeString r1 enc testString mempty

    r2 <- passThrough
    _ <- writeString r2 enc testString mempty
    setEncoding r2 enc

    onData r1 \buf -> unsafePartial do
      onDataEither r2 \(Left str) -> do
        _ <- assertEqual <$> Buffer.toString enc buf <*> pure testString
        assertEqual str testString

testPipe :: Effect Boolean
testPipe = do
  sIn <- passThrough
  sOut <- passThrough
  zip <- createGzip
  unzip <- createGunzip

  log "pipe 1"
  _ <- sIn `pipe` zip
  log "pipe 2"
  _ <- zip `pipe` unzip
  log "pipe 3"
  _ <- unzip `pipe` sOut

  writeString sIn UTF8 testString \_ -> do
    end sIn \_ -> do
      onDataString sOut UTF8 \str -> do
        assertEqual str testString

testPipeline :: Effect Boolean
testPipeline = do
  sIn <- passThrough
  sOut <- passThrough
  zip <- createGzip
  unzip <- createGunzip

  pipeline sIn [ zip, unzip ] sOut mempty

  onDataString sOut UTF8 \str ->
    assertEqual str testString

  writeString sIn UTF8 testString \_ -> do
    end sIn mempty

foreign import createGzip :: Effect Duplex
foreign import createGunzip :: Effect Duplex

testWrite :: Effect Unit
testWrite = do
  hasError
  noError
  where
  hasError = do
    w1 <- passThrough
    _ <- onError w1 (const $ pure unit)
    void $ end w1 $ const $ pure unit
    void $ writeString w1 UTF8 "msg" \err -> do
      assert' "writeString - should have error" $ isJust err

  noError = do
    w1 <- passThrough
    void $ writeString w1 UTF8 "msg1" \err -> do
      assert' "writeString - should have no error" $ isNothing err
    void $ end w1 (const $ pure unit)

testEnd :: Effect Unit
testEnd = do
  hasError
  noError
  where
  hasError = do
    w1 <- passThrough
    _ <- onError w1 (const $ pure unit)
    void $ writeString w1 UTF8 "msg" \_ -> do
      _ <- destroyWithError w1 $ error "Problem"
      end w1 \err -> do
        assert' "end - should have error" $ isJust err

  noError = do
    w1 <- passThrough
    end w1 \err -> do
      assert' "end - should have no error" $ isNothing err
