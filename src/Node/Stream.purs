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
  , writeString
  , cork
  , uncork
  , setDefaultEncoding
  , end
  , destroy
  , destroyWithError
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Nullable as N
import Effect (Effect)
import Effect.Exception (Error, throw)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding)
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

foreign import setEncodingImpl
  :: forall w
   . Readable w
  -> String
  -> Effect Unit

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
setEncoding r enc = setEncodingImpl r (show enc)

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
foreign import resume :: forall w. Readable w -> Effect Unit

-- | Pause reading from the stream.
foreign import pause :: forall w. Readable w -> Effect Unit

-- | Check whether or not a stream is paused for reading.
foreign import isPaused :: forall w. Readable w -> Effect Boolean

-- | Read chunks from a readable stream and write them to a writable stream.
foreign import pipe
  :: forall r w
   . Readable w
  -> Writable r
  -> Effect (Writable r)

-- | Detach a Writable stream previously attached using `pipe`.
foreign import unpipe
  :: forall r w
   . Readable w
  -> Writable r
  -> Effect Unit

-- | Detach all Writable streams previously attached using `pipe`.
foreign import unpipeAll
  :: forall w
   . Readable w
  -> Effect Unit

foreign import writeImpl
  :: forall r
   . Writable r
  -> Buffer
  -> EffectFn1 (N.Nullable Error) Unit
  -> Effect Boolean

-- | Write a Buffer to a writable stream.
write
  :: forall r
   . Writable r
  -> Buffer
  -> (Maybe Error -> Effect Unit)
  -> Effect Boolean
write w b cb = writeImpl w b $ mkEffectFn1 (cb <<< N.toMaybe)

foreign import writeStringImpl
  :: forall r
   . Writable r
  -> String
  -> String
  -> EffectFn1 (N.Nullable Error) Unit
  -> Effect Boolean

-- | Write a string in the specified encoding to a writable stream.
writeString
  :: forall r
   . Writable r
  -> Encoding
  -> String
  -> (Maybe Error -> Effect Unit)
  -> Effect Boolean
writeString w enc s cb = writeStringImpl w (show enc) s $ mkEffectFn1 (cb <<< N.toMaybe)

-- | Force buffering of writes.
foreign import cork :: forall r. Writable r -> Effect Unit

-- | Flush buffered data.
foreign import uncork :: forall r. Writable r -> Effect Unit

foreign import setDefaultEncodingImpl
  :: forall r
   . Writable r
  -> String
  -> Effect Unit

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
setDefaultEncoding r enc = setDefaultEncodingImpl r (show enc)

foreign import endImpl
  :: forall r
   . Writable r
  -> EffectFn1 (N.Nullable Error) Unit
  -> Effect Unit

-- | End writing data to the stream.
end
  :: forall r
   . Writable r
  -> (Maybe Error -> Effect Unit)
  -> Effect Unit
end w cb = endImpl w $ mkEffectFn1 (cb <<< N.toMaybe)

-- | Destroy the stream. It will release any internal resources.
--
-- Added in node 8.0.
foreign import destroy
  :: forall r
   . Stream r
  -> Effect Unit

-- | Destroy the stream and emit 'error'.
--
-- Added in node 8.0.
foreign import destroyWithError
  :: forall r
   . Stream r
  -> Error
  -> Effect Unit
