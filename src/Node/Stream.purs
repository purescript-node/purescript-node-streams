-- | This module provides a low-level wrapper for the [Node Stream API](https://nodejs.org/api/stream.html).

module Node.Stream
  ( Stream
  , Read
  , Readable
  , Write
  , Writable
  , Duplex
  , toEventEmitter
  , setEncoding
  , closeH
  , errorH
  , drainH
  , finishH
  , pipeH
  , unpipeH
  , Chunk
  , dataH
  , dataHStr
  , dataHEither
  , pauseH
  , readableH
  , resumeH
  , endH
  , resume
  , pause
  , isPaused
  , pipe
  , unpipe
  , unpipeAll
  , read
  , read'
  , readString
  , readString'
  , readEither
  , readEither'
  , write
  , write'
  , writeString
  , writeString'
  , cork
  , uncork
  , setDefaultEncoding
  , end
  , end'
  , destroy
  , destroy'
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Exception (Error, throw)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding, encodingToNode)
import Node.EventEmitter (EventEmitter, EventHandle(..))
import Node.EventEmitter.UtilTypes (EventHandle1, EventHandle0)
import Unsafe.Coerce (unsafeCoerce)

-- | A stream.
-- |
-- | The type arguments track, in order:
-- |
-- | - Whether reading and/or writing from/to the stream are allowed.
-- | - Effects associated with reading/writing from/to this stream.
foreign import data Stream :: Row Type -> Type

-- | A phantom type associated with _readable streams_.
data Read

-- | A readable stream.
type Readable r = Stream (read :: Read | r)

-- | A phantom type associated with _writable streams_.
data Write

-- | A writable stream.
type Writable r = Stream (write :: Write | r)

-- | A duplex (readable _and_ writable stream)
type Duplex = Stream (read :: Read, write :: Write)

toEventEmitter :: forall rw. Stream rw -> EventEmitter
toEventEmitter = unsafeCoerce

-- | Internal type. This should not be used by end-users.
foreign import data Chunk :: Type

foreign import readChunkImpl
  :: forall r
   . EffectFn3
       (EffectFn1 Buffer r)
       (EffectFn1 String r)
       Chunk
       r

-- | Listen for `data` events, returning data in a Buffer. Note that this will fail
-- | if `setEncoding` has been called on the stream.
-- |
-- | This is likely the handler you want to use for converting a `Stream` into a `String`:
-- | ```
-- | let useStringCb = ...
-- | ref <- Ref.new
-- | stream # on dataH \buf ->
-- |   Ref.modify_ (\ref' -> Array.snoc ref' buf) ref
-- | stream # on endH do
-- |   bufs <- Ref.read ref
-- |   useStringCb $ Buffer.toString UTF8 $ Buffer.concat bufs
-- | ```
dataH :: forall w. EventHandle (Readable w) (Buffer -> Effect Unit) (EffectFn1 Chunk Unit)
dataH = EventHandle "data" \cb ->
  mkEffectFn1 \chunk -> do
    runEffectFn3
      readChunkImpl
      (mkEffectFn1 cb)
      (mkEffectFn1 \_ -> throw "Got a String, not a Buffer. Stream encoding should not be set")
      chunk

-- | Listen for `data` events, returning data as a String. Note that this will fail
-- | if `setEncoding` has NOT been called on the stream.
dataHStr :: forall w. EventHandle (Readable w) (String -> Effect Unit) (EffectFn1 Chunk Unit)
dataHStr = EventHandle "data" \cb ->
  mkEffectFn1 \chunk -> do
    runEffectFn3
      readChunkImpl
      (mkEffectFn1 \_ -> throw "Got a Buffer, not String. Stream encoding must be set to get a String.")
      (mkEffectFn1 cb)
      chunk

-- | Listen for `data` events, returning data in a Buffer or String. This will work
-- | regardless of whether `setEncoding` has been called or not.
dataHEither :: forall w. EventHandle (Readable w) (Either Buffer String -> Effect Unit) (EffectFn1 Chunk Unit)
dataHEither = EventHandle "data" \cb ->
  mkEffectFn1 \chunk -> do
    runEffectFn3
      readChunkImpl
      (mkEffectFn1 (cb <<< Left))
      (mkEffectFn1 (cb <<< Right))
      chunk

-- | Note: this will fail if `setEncoding` has been called on the stream.
read :: forall w. Readable w -> Effect (Maybe Buffer)
read r = do
  chunk <- runEffectFn1 readImpl r
  case toMaybe chunk of
    Nothing ->
      pure Nothing
    Just c ->
      runEffectFn3 readChunkImpl
        (mkEffectFn1 \buf -> pure $ Just buf)
        (mkEffectFn1 \_ -> throw "Stream encoding should not be set")
        c

foreign import readImpl :: forall w. EffectFn1 (Readable w) (Nullable Chunk)

-- | Note: this will fail if `setEncoding` has been called on the stream.
read' :: forall w. Readable w -> Int -> Effect (Maybe Buffer)
read' r size = do
  chunk <- runEffectFn2 readSizeImpl r size
  case toMaybe chunk of
    Nothing ->
      pure Nothing
    Just c ->
      runEffectFn3 readChunkImpl
        (mkEffectFn1 \buf -> pure $ Just buf)
        (mkEffectFn1 \_ -> throw "Stream encoding should not be set")
        c

foreign import readSizeImpl :: forall w. EffectFn2 (Readable w) (Int) (Nullable Chunk)

-- | Reads the stream to get a Buffer and converts that into a String
-- | with the given encoding.
-- | Note: this will fail if `setEncoding` has been called on the stream.
-- | If that is the case, use `readEither` instead.
readString
  :: forall w
   . Readable w
  -> Encoding
  -> Effect (Maybe String)
readString r enc = do
  mbBuf <- read r
  case mbBuf of
    Nothing ->
      pure Nothing
    Just buf -> do
      Just <$> Buffer.toString enc buf

-- | Reads the given number of bytes from the stream to get a Buffer
-- | and converts that into a String with the given encoding.
-- | Note: this will fail if `setEncoding` has been called on the stream.
-- | If that is the case, use `readEither'` instead.
readString'
  :: forall w
   . Readable w
  -> Int
  -> Encoding
  -> Effect (Maybe String)
readString' r size enc = do
  mbBuf <- read' r size
  case mbBuf of
    Nothing ->
      pure Nothing
    Just buf -> do
      Just <$> Buffer.toString enc buf

-- | Reads a chunk from the stream. This will work whether or not
-- | `setEncoding` has been called on the stream.
readEither :: forall w. Readable w -> Effect (Maybe (Either String Buffer))
readEither r = do
  chunk <- runEffectFn1 readImpl r
  case toMaybe chunk of
    Nothing ->
      pure Nothing
    Just c ->
      runEffectFn3 readChunkImpl
        (mkEffectFn1 (pure <<< Just <<< Right))
        (mkEffectFn1 (pure <<< Just <<< Left))
        c

-- | Reads the given number of bytes from the stream. This will work whether or not
-- | `setEncoding` has been called on the stream.
readEither' :: forall w. Readable w -> Int -> Effect (Maybe (Either String Buffer))
readEither' r size = do
  chunk <- runEffectFn2 readSizeImpl r size
  case toMaybe chunk of
    Nothing ->
      pure Nothing
    Just c ->
      runEffectFn3 readChunkImpl
        (mkEffectFn1 (pure <<< Just <<< Right))
        (mkEffectFn1 (pure <<< Just <<< Left))
        c

-- | Set the encoding used to read chunks as strings from the stream. This
-- | function may be useful when you are passing a readable stream to some other
-- | JavaScript library, which already expects an encoding to be set.
-- |
-- | Where possible, you should try to use `onDataString` instead of this
-- | function.
setEncoding
  :: forall w
   . Readable w
  -> Encoding
  -> Effect Unit
setEncoding r enc = runEffectFn2 setEncodingImpl r (show enc)

foreign import setEncodingImpl :: forall w. EffectFn2 (Readable w) String Unit

closeH :: forall rw. EventHandle0 (Stream rw)
closeH = EventHandle "close" identity

errorH :: forall rw. EventHandle1 (Stream rw) Error
errorH = EventHandle "error" mkEffectFn1

drainH :: forall r. EventHandle0 (Writable r)
drainH = EventHandle "drain" identity

finishH :: forall r. EventHandle0 (Writable r)
finishH = EventHandle "finish" identity

pipeH :: forall r w. EventHandle1 (Writable r) (Readable w)
pipeH = EventHandle "pipe" mkEffectFn1

unpipeH :: forall r w. EventHandle1 (Writable r) (Readable w)
unpipeH = EventHandle "unpipe" mkEffectFn1

pauseH :: forall w. EventHandle0 (Readable w)
pauseH = EventHandle "pause" identity

readableH :: forall w. EventHandle0 (Readable w)
readableH = EventHandle "readable" identity

resumeH :: forall w. EventHandle0 (Readable w)
resumeH = EventHandle "resume" identity

endH :: forall w. EventHandle0 (Readable w)
endH = EventHandle "end" identity

-- | Resume reading from the stream.
resume :: forall w. Readable w -> Effect Unit
resume r = runEffectFn1 resumeImpl r

foreign import resumeImpl :: forall w. EffectFn1 (Readable w) (Unit)

-- | Pause reading from the stream.
pause :: forall w. Readable w -> Effect Unit
pause r = runEffectFn1 pauseImpl r

foreign import pauseImpl :: forall w. EffectFn1 (Readable w) (Unit)

-- | Check whether or not a stream is paused for reading.
isPaused :: forall w. Readable w -> Effect Boolean
isPaused r = runEffectFn1 isPausedImpl r

foreign import isPausedImpl :: forall w. EffectFn1 (Readable w) (Boolean)

-- | Read chunks from a readable stream and write them to a writable stream.
pipe :: forall w r. Readable w -> Writable r -> Effect Unit
pipe r w = runEffectFn2 pipeImpl r w

foreign import pipeImpl :: forall w r. EffectFn2 (Readable w) (Writable r) (Unit)

-- | Detach a Writable stream previously attached using `pipe`.
unpipe :: forall w r. Readable w -> Writable r -> Effect Unit
unpipe r w = runEffectFn2 unpipeImpl r w

foreign import unpipeImpl :: forall w r. EffectFn2 (Readable w) (Writable r) (Unit)

-- | Detach all Writable streams previously attached using `pipe`.
unpipeAll :: forall w. Readable w -> Effect Unit
unpipeAll r = runEffectFn1 unpipeAllImpl r

foreign import unpipeAllImpl :: forall w. EffectFn1 (Readable w) (Unit)

write :: forall r. Writable r -> Buffer -> Effect Boolean
write w b = runEffectFn2 writeImpl w b

foreign import writeImpl :: forall r a. EffectFn2 (Writable r) (Buffer) (a)

write' :: forall r. Writable r -> Buffer -> (Maybe Error -> Effect Unit) -> Effect Boolean
write' w b cb = runEffectFn3 writeCbImpl w b $ mkEffectFn1 \err -> cb (toMaybe err)

foreign import writeCbImpl :: forall r a. EffectFn3 (Writable r) (Buffer) (EffectFn1 (Nullable Error) Unit) (a)

writeString :: forall r. Writable r -> Encoding -> String -> Effect Boolean
writeString w enc str = runEffectFn3 writeStringImpl w str (encodingToNode enc)

foreign import writeStringImpl :: forall r a. EffectFn3 (Writable r) (String) (String) (a)

writeString' :: forall r. Writable r -> Encoding -> String -> (Maybe Error -> Effect Unit) -> Effect Boolean
writeString' w enc str cb = runEffectFn4 writeStringCbImpl w str (encodingToNode enc) $ mkEffectFn1 \err -> cb (toMaybe err)

foreign import writeStringCbImpl :: forall r a. EffectFn4 (Writable r) (String) (String) (EffectFn1 (Nullable Error) Unit) (a)

-- | Force buffering of writes.
cork :: forall r. Writable r -> Effect Unit
cork s = runEffectFn1 corkImpl s

foreign import corkImpl :: forall r. EffectFn1 (Writable r) (Unit)

-- | Flush buffered data.
uncork :: forall r. Writable r -> Effect Unit
uncork w = runEffectFn1 uncorkImpl w

foreign import uncorkImpl :: forall r. EffectFn1 (Writable r) (Unit)

-- | Set the default encoding used to write strings to the stream. This function
-- | is useful when you are passing a writable stream to some other JavaScript
-- | library, which already expects a default encoding to be set. It has no
-- | effect on the behaviour of the `writeString` function (because that
-- | function ensures that the encoding is always supplied explicitly).
setDefaultEncoding
  :: forall r
   . Writable r
  -> Encoding
  -> Effect Unit
setDefaultEncoding r enc = runEffectFn2 setDefaultEncodingImpl r (show enc)

foreign import setDefaultEncodingImpl :: forall r. EffectFn2 (Writable r) String Unit

-- | End writing data to the stream.
end :: forall r. Writable r -> Effect Unit
end w = runEffectFn1 endImpl w

foreign import endImpl :: forall r. EffectFn1 (Writable r) (Unit)

end' :: forall r. Writable r -> (Maybe Error -> Effect Unit) -> Effect Unit
end' w cb = runEffectFn2 endCbImpl w $ mkEffectFn1 \err -> cb (toMaybe err)

foreign import endCbImpl :: forall r. EffectFn2 (Writable r) (EffectFn1 (Nullable Error) Unit) (Unit)

destroy :: forall r. Stream r -> Effect Unit
destroy w = runEffectFn1 destroyImpl w

foreign import destroyImpl :: forall r. EffectFn1 (Stream r) (Unit)

destroy' :: forall r. Stream r -> Error -> Effect Unit
destroy' w e = runEffectFn2 destroyErrorImpl w e

foreign import destroyErrorImpl :: forall r. EffectFn2 (Stream r) (Error) Unit

