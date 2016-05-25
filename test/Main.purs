module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), isNothing, isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..))
import Node.Buffer as Buffer
import Node.Encoding
import Node.Stream

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Test.Assert

assertEqual :: forall e a. (Show a, Eq a) => a -> a -> Eff (assert :: ASSERT | e) Unit
assertEqual x y =
  assert' (show x <> " did not equal " <> show y) (x == y)

foreign import data STREAM_BUFFER :: !

foreign import writableStreamBuffer :: forall eff. Eff (sb :: STREAM_BUFFER | eff) (Writable () (sb :: STREAM_BUFFER | eff))

foreign import getContentsAsString :: forall r eff. Writable r (sb :: STREAM_BUFFER | eff) -> Eff (sb :: STREAM_BUFFER | eff) String

foreign import readableStreamBuffer :: forall eff. Eff (sb :: STREAM_BUFFER | eff) (Readable () (sb :: STREAM_BUFFER | eff))

foreign import putImpl :: forall r eff. String -> String -> Readable r (sb :: STREAM_BUFFER | eff) -> Eff (sb :: STREAM_BUFFER | eff) Unit

put :: forall r eff. String -> Encoding -> Readable r (sb :: STREAM_BUFFER | eff) -> Eff (sb :: STREAM_BUFFER | eff) Unit
put str enc = putImpl str (show enc)

main = do
  log "setDefaultEncoding should not affect writing"
  testSetDefaultEncoding

  log "setEncoding should not affect reading"
  testSetEncoding

  log "test pipe"
  testPipe

  log "test manual reads"
  testReads

testString :: String
testString = "üöß💡"

testReads = do
  testReadString
  testReadBuf

  where
    testReadString = do
      sIn <- passThrough
      v   <- readString sIn Nothing UTF8
      assert (isNothing v)

      onReadable sIn do
        str <- readString sIn Nothing UTF8
        assert (isJust str)
        assertEqual (fromJust str) testString
        return unit

      writeString sIn UTF8 testString do
        return unit

    testReadBuf = do
      sIn <- passThrough
      v   <- read sIn Nothing
      assert (isNothing v)

      onReadable sIn do
        buf <- read sIn Nothing
        assert (isJust buf)
        assertEqual <$> (Buffer.toString UTF8 (fromJust buf))
                    <*> pure testString
        return unit

      writeString sIn UTF8 testString do
        return unit

testSetDefaultEncoding = do
  w1 <- writableStreamBuffer
  check w1

  w2 <- writableStreamBuffer
  setDefaultEncoding w2 UCS2
  check w2

  where
  check w = do
    writeString w UTF8 testString do
      c <- getContentsAsString w
      assertEqual testString c

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

    onData r1 \buf -> do
      onDataEither r2 \(Left str) -> do
        assertEqual <$> Buffer.toString enc buf <*> pure testString
        assertEqual str testString

testPipe = do
  sIn   <- passThrough
  sOut  <- passThrough
  zip   <- createGzip
  unzip <- createGunzip

  log "pipe 1"
  sIn `pipe` zip
  log "pipe 2"
  zip `pipe` unzip
  log "pipe 3"
  unzip `pipe` sOut

  writeString sIn UTF8 testString do
    end sIn do
      onDataString sOut UTF8 \str -> do
        assertEqual str testString

foreign import data GZIP :: !

foreign import createGzip :: forall eff. Eff (gzip :: GZIP | eff) (Duplex (gzip :: GZIP | eff))
foreign import createGunzip :: forall eff. Eff (gzip :: GZIP | eff) (Duplex (gzip :: GZIP | eff))

foreign import data PASS_THROUGH :: !

-- | Create a PassThrough stream, which simply writes its input to its output.
foreign import passThrough :: forall eff. Eff (stream :: PASS_THROUGH | eff) (Duplex (stream :: PASS_THROUGH | eff))
