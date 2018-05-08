module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.Stream (Duplex, Readable, Writable, onDataString, end, writeString, pipe, onDataEither, onData, setEncoding, setDefaultEncoding, read, onReadable, readString)
import Test.Assert (ASSERT, assert, assert')
import Node.Buffer as Buffer
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, isNothing, isJust)
import Partial.Unsafe (unsafePartial)

assertEqual :: forall e a. Show a => Eq a => a -> a -> Eff (assert :: ASSERT | e) Unit
assertEqual x y =
  assert' (show x <> " did not equal " <> show y) (x == y)


foreign import writableStreamBuffer :: forall eff. Eff (sb :: STREAM_BUFFER | eff) (Writable () (sb :: STREAM_BUFFER | eff))

foreign import getContentsAsString :: forall r eff. Writable r (sb :: STREAM_BUFFER | eff) -> Eff (sb :: STREAM_BUFFER | eff) String

foreign import readableStreamBuffer :: forall eff. Eff (sb :: STREAM_BUFFER | eff) (Readable () (sb :: STREAM_BUFFER | eff))

foreign import putImpl :: forall r eff. String -> String -> Readable r (sb :: STREAM_BUFFER | eff) -> Eff (sb :: STREAM_BUFFER | eff) Unit

put :: forall r eff. String -> Encoding -> Readable r (sb :: STREAM_BUFFER | eff) -> Eff (sb :: STREAM_BUFFER | eff) Unit
put str enc = putImpl str (show enc)

main
  :: forall eff
   . Eff ( console :: CONSOLE
         , sb :: STREAM_BUFFER
         , assert :: ASSERT
         , exception :: EXCEPTION
         , buffer :: Buffer.BUFFER
         , stream :: PASS_THROUGH
         , gzip :: GZIP
         | eff ) Boolean
main = do
  log "setDefaultEncoding should not affect writing"
  _ <- testSetDefaultEncoding

  log "setEncoding should not affect reading"
  testSetEncoding

  log "test pipe"
  _ <- testPipe

  log "test manual reads"
  testReads

testString :: String
testString = "üöß💡"

testReads
  :: forall eff
   . Eff ( stream :: PASS_THROUGH
         , exception :: EXCEPTION
         , buffer :: Buffer.BUFFER
         , assert :: ASSERT
         | eff ) Boolean
testReads = do
  _ <- testReadString
  testReadBuf

  where
    testReadString = do
      sIn <- passThrough
      v   <- readString sIn Nothing UTF8
      assert (isNothing v)

      onReadable sIn do
        str <- readString sIn Nothing UTF8
        assert (isJust str)
        assertEqual (unsafePartial (fromJust str)) testString
        pure unit

      writeString sIn UTF8 testString do
        pure unit

    testReadBuf = do
      sIn <- passThrough
      v   <- read sIn Nothing
      assert (isNothing v)

      onReadable sIn do
        buf <- read sIn Nothing
        assert (isJust buf)
        _ <- assertEqual <$> (Buffer.toString UTF8 (unsafePartial (fromJust buf)))
                    <*> pure testString
        pure unit

      writeString sIn UTF8 testString do
        pure unit

testSetDefaultEncoding
  :: forall eff
   . Eff ( sb :: STREAM_BUFFER
         , assert :: ASSERT
         | eff ) Boolean
testSetDefaultEncoding = do
  w1 <- writableStreamBuffer
  _ <- check w1

  w2 <- writableStreamBuffer
  setDefaultEncoding w2 UCS2
  check w2

  where
  check w = do
    writeString w UTF8 testString do
      c <- getContentsAsString w
      assertEqual testString c

testSetEncoding
  :: forall eff
   . Eff ( sb :: STREAM_BUFFER
         , exception :: EXCEPTION
         , buffer :: Buffer.BUFFER
         , assert :: ASSERT
         | eff ) Unit
testSetEncoding = do
  check UTF8
  check UTF16LE
  check UCS2
  where
  check enc = do
    r1 <- readableStreamBuffer
    put testString enc r1

    r2 <- readableStreamBuffer
    put testString enc r2
    setEncoding r2 enc

    onData r1 \buf -> unsafePartial do
      onDataEither r2 \(Left str) -> do
        _ <- assertEqual <$> Buffer.toString enc buf <*> pure testString
        assertEqual str testString

testPipe
  :: forall eff
   . Eff ( stream :: PASS_THROUGH
         , gzip :: GZIP
         , exception :: EXCEPTION
         , assert :: ASSERT
         , console :: CONSOLE
         | eff ) Boolean
testPipe = do
  sIn   <- passThrough
  sOut  <- passThrough
  zip   <- createGzip
  unzip <- createGunzip

  log "pipe 1"
  _ <- sIn `pipe` zip
  log "pipe 2"
  _ <- zip `pipe` unzip
  log "pipe 3"
  _ <- unzip `pipe` sOut

  writeString sIn UTF8 testString do
    end sIn do
      onDataString sOut UTF8 \str -> do
        assertEqual str testString


foreign import createGzip :: forall eff. Eff (gzip :: GZIP | eff) (Duplex (gzip :: GZIP | eff))
foreign import createGunzip :: forall eff. Eff (gzip :: GZIP | eff) (Duplex (gzip :: GZIP | eff))


-- | Create a PassThrough stream, which simply writes its input to its output.
foreign import passThrough :: forall eff. Eff (stream :: PASS_THROUGH | eff) (Duplex (stream :: PASS_THROUGH | eff))
