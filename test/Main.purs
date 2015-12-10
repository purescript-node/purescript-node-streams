module Test.Main where

import Prelude

import Node.Buffer as Buffer
import Node.Encoding
import Node.Stream
import Node.Stream.StdIO

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

testString :: String
testString = "Liebe Grüße\nBergentrückung\n💡"

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
  check readableStreamBuffer

  check do
    r2 <- readableStreamBuffer
    setEncoding r2 UTF8
    pure r2

  check do
    r3 <- readableStreamBuffer
    setEncoding r3 UCS2
    pure r3

  where
  check makeR = do
    r1 <- makeR
    put testString UTF8 r1

    r2 <- makeR
    put testString UTF8 r2

    onData r1 \buf -> do
      onDataString r2 UTF8 \str -> do
        assertEqual <$> Buffer.toString UTF8 buf <*> pure testString
        assertEqual str testString
        log "ok."
