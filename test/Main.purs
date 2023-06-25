module Test.Main where

import Prelude

import Data.Maybe (fromJust, isJust, isNothing)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.Stream (Duplex, dataH, dataHStr, destroy', end, end', errorH, newPassThrough, pipe, read, readString, readableH, setDefaultEncoding, setEncoding, writeString'_, writeString_)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assert')

assertEqual :: forall a. Show a => Eq a => a -> a -> Effect Unit
assertEqual x y =
  assert' (show x <> " did not equal " <> show y) (x == y)

main :: Effect Unit
main = do
  log "setDefaultEncoding should not affect writing"
  testSetDefaultEncoding

  log "setEncoding should not affect reading"
  testSetEncoding

  log "test pipe"
  testPipe

  log "test write"
  testWrite

  log "test end"
  testEnd

  log "test manual reads"
  testReads

testString :: String
testString = "üöß💡"

testReads :: Effect Unit
testReads = do
  testReadString
  testReadBuf

  where
  testReadString = do
    sIn <- newPassThrough
    v <- readString sIn UTF8
    assert (isNothing v)

    sIn # on_ readableH do
      str <- readString sIn UTF8
      assert (isJust str)
      assertEqual (unsafePartial (fromJust str)) testString
      pure unit

    writeString_ sIn UTF8 testString

  testReadBuf = do
    sIn <- newPassThrough
    v <- read sIn
    assert (isNothing v)

    sIn # on_ readableH do
      buf <- read sIn
      assert (isJust buf)
      _ <- assertEqual <$> (Buffer.toString UTF8 (unsafePartial (fromJust buf)))
        <*> pure testString
      pure unit

    writeString_ sIn UTF8 testString

testSetDefaultEncoding :: Effect Unit
testSetDefaultEncoding = do
  w1 <- newPassThrough
  check w1

  w2 <- newPassThrough
  setDefaultEncoding w2 UCS2
  check w2

  where
  check w = do
    w # on_ dataH \buf -> do
      str <- Buffer.toString UTF8 buf
      assertEqual testString str
    writeString_ w UTF8 testString

testSetEncoding :: Effect Unit
testSetEncoding = do
  check UTF8
  check UTF16LE
  check UCS2
  where
  check enc = do
    r1 <- newPassThrough
    writeString_ r1 enc testString

    r2 <- newPassThrough
    writeString_ r1 enc testString
    setEncoding r2 enc

    r1 # on_ dataH \buf -> do
      r2 # on_ dataHStr \str -> do
        join $ assertEqual <$> Buffer.toString enc buf <*> pure testString
        assertEqual str testString

testPipe :: Effect Unit
testPipe = do
  sIn <- newPassThrough
  sOut <- newPassThrough
  zip <- createGzip
  unzip <- createGunzip

  log "pipe 1"
  _ <- sIn `pipe` zip
  log "pipe 2"
  _ <- zip `pipe` unzip
  log "pipe 3"
  _ <- unzip `pipe` sOut

  writeString'_ sIn UTF8 testString \_ -> do
    end' sIn \_ -> do
      sOut # on_ dataH \buf -> do
        str <- Buffer.toString UTF8 buf
        assertEqual str testString

foreign import createGzip :: Effect Duplex
foreign import createGunzip :: Effect Duplex

testWrite :: Effect Unit
testWrite = do
  hasError
  noError
  where
  hasError = do
    w1 <- newPassThrough
    w1 # on_ errorH (const $ pure unit)
    end w1
    writeString'_ w1 UTF8 "msg" \err -> do
      assert' "writeString - should have error" $ isJust err

  noError = do
    w1 <- newPassThrough
    writeString'_ w1 UTF8 "msg1" \err -> do
      assert' "writeString - should have no error" $ isNothing err
    end w1

testEnd :: Effect Unit
testEnd = do
  hasError
  noError
  where
  hasError = do
    w1 <- newPassThrough
    w1 # on_ errorH (const $ pure unit)
    writeString'_ w1 UTF8 "msg" \_ -> do
      _ <- destroy' w1 $ error "Problem"
      end' w1 \err -> do
        assert' "end - should have error" $ isJust err

  noError = do
    w1 <- newPassThrough
    end' w1 \err -> do
      assert' "end - should have no error" $ isNothing err
