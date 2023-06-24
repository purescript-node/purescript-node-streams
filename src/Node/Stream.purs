-- | This module provides a low-level wrapper for the [Node Stream API](https://nodejs.org/api/stream.html).

module Node.Stream
  ( Stream
  , Read
  , Readable
  , Write
  , Writable
  , Duplex
  , toEventEmitter
  , onData
  , onDataString
  , onDataEither
  , setEncoding
  , closeH
  , errorH
  , drainH
  , finishH
  , pipeH
  , unpipeH
  , pauseH
  , readableH
  , resumeH
  , endH
  , readable
  , readableEnded
  , readableFlowing
  , readableHighWaterMark
  , readableLength
  , resume
  , pause
  , isPaused
  , pipe
  , pipe'
  , unpipe
  , unpipeAll
  , read
  , readString
  , readEither
  , writeable
  , writeableEnded
  , writeableCorked
  , errored
  , writeableFinished
  , writeableHighWaterMark
  , writeableLength
  , writeableNeedDrain
  , write
  , write_
  , writeCb
  , writeCb_
  , writeString
  , writeString_
  , writeStringCb
  , writeStringCb_
  , cork
  , uncork
  , setDefaultEncoding
  , end
  , end_
  , destroy
  , destroy'
  , closed
  , destroyed
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
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

foreign import undefined :: forall a. a

foreign import data Chunk :: Type

foreign import readChunkImpl
  :: (forall l r. l -> Either l r)
  -> (forall l r. r -> Either l r)
  -> Chunk
  -> Either String Buffer

readChunk :: Chunk -> Either String Buffer
readChunk = readChunkImpl Left Right

-- | Listen for `data` events, returning data in a Buffer. Note that this will fail
-- | if `setEncoding` has been called on the stream.
onData
  :: forall w
   . Readable w
  -> (Buffer -> Effect Unit)
  -> Effect Unit
onData r cb =
  onDataEither r (cb <=< fromEither)
  where
  fromEither x =
    case x of
      Left _ ->
        throw "Stream encoding should not be set"
      Right buf ->
        pure buf

read
  :: forall w
   . Readable w
  -> Maybe Int
  -> Effect (Maybe Buffer)
read r size = do
  v <- readEither r size
  case v of
    Nothing -> pure Nothing
    Just (Left _) -> throw "Stream encoding should not be set"
    Just (Right b) -> pure (Just b)

readString
  :: forall w
   . Readable w
  -> Maybe Int
  -> Encoding
  -> Effect (Maybe String)
readString r size enc = do
  v <- readEither r size
  case v of
    Nothing -> pure Nothing
    Just (Left _) -> throw "Stream encoding should not be set"
    Just (Right buf) -> Just <$> Buffer.toString enc buf

readEither
  :: forall w
   . Readable w
  -> Maybe Int
  -> Effect (Maybe (Either String Buffer))
readEither r size = readImpl readChunk Nothing Just r (fromMaybe undefined size)

foreign import readImpl
  :: forall r
   . (Chunk -> Either String Buffer)
  -> (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Readable r
  -> Int
  -> Effect (Maybe (Either String Buffer))

-- | Listen for `data` events, returning data in a String, which will be
-- | decoded using the given encoding. Note that this will fail if `setEncoding`
-- | has been called on the stream.
onDataString
  :: forall w
   . Readable w
  -> Encoding
  -> (String -> Effect Unit)
  -> Effect Unit
onDataString r enc cb = onData r (cb <=< Buffer.toString enc)

-- | Listen for `data` events, returning data in an `Either String Buffer`. This
-- | function is provided for the (hopefully rare) case that `setEncoding` has
-- | been called on the stream.
onDataEither
  :: forall r
   . Readable r
  -> (Either String Buffer -> Effect Unit)
  -> Effect Unit
onDataEither r cb = onDataEitherImpl readChunk r cb

foreign import onDataEitherImpl
  :: forall r
   . (Chunk -> Either String Buffer)
  -> Readable r
  -> (Either String Buffer -> Effect Unit)
  -> Effect Unit

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

readable :: forall w. Readable w -> Effect Boolean
readable r = runEffectFn1 readableImpl r

foreign import readableImpl :: forall w. EffectFn1 (Readable w) (Boolean)

readableEnded :: forall w. Readable w -> Effect Boolean
readableEnded r = runEffectFn1 readableEndedImpl r

foreign import readableEndedImpl :: forall w. EffectFn1 (Readable w) (Boolean)

readableFlowing :: forall w. Readable w -> Effect Boolean
readableFlowing r = runEffectFn1 readableFlowingImpl r

foreign import readableFlowingImpl :: forall w. EffectFn1 (Readable w) (Boolean)

readableHighWaterMark :: forall w. Readable w -> Effect Boolean
readableHighWaterMark r = runEffectFn1 readableHighWaterMarkImpl r

foreign import readableHighWaterMarkImpl :: forall w. EffectFn1 (Readable w) (Boolean)

readableLength :: forall w. Readable w -> Effect Boolean
readableLength r = runEffectFn1 readableLengthImpl r

foreign import readableLengthImpl :: forall w. EffectFn1 (Readable w) (Boolean)

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

pipe' :: forall w r. Readable w -> Writable r -> { end :: Boolean } -> Effect Unit
pipe' r w o = runEffectFn3 pipeCbImpl r w o

foreign import pipeCbImpl :: forall w r. EffectFn3 (Readable w) (Writable r) ({ end :: Boolean }) (Unit)

-- | Detach a Writable stream previously attached using `pipe`.
unpipe :: forall w r. Readable w -> Writable r -> Effect Unit
unpipe r w = runEffectFn2 unpipeImpl r w

foreign import unpipeImpl :: forall w r. EffectFn2 (Readable w) (Writable r) (Unit)

-- | Detach all Writable streams previously attached using `pipe`.
unpipeAll :: forall w. Readable w -> Effect Unit
unpipeAll r = runEffectFn1 unpipeAllImpl r

foreign import unpipeAllImpl :: forall w. EffectFn1 (Readable w) (Unit)

writeable :: forall r. Writable r -> Effect Boolean
writeable w = runEffectFn1 writeableImpl w

foreign import writeableImpl :: forall r. EffectFn1 (Writable r) (Boolean)

writeableEnded :: forall r. Writable r -> Effect Boolean
writeableEnded w = runEffectFn1 writeableEndedImpl w

foreign import writeableEndedImpl :: forall r. EffectFn1 (Writable r) (Boolean)

writeableCorked :: forall r. Writable r -> Effect Boolean
writeableCorked w = runEffectFn1 writeableCorkedImpl w

foreign import writeableCorkedImpl :: forall r. EffectFn1 (Writable r) (Boolean)

errored :: forall rw. Stream rw -> Effect Boolean
errored rw = runEffectFn1 erroredImpl rw

foreign import erroredImpl :: forall rw. EffectFn1 (Stream rw) (Boolean)

writeableFinished :: forall r. Writable r -> Effect Boolean
writeableFinished w = runEffectFn1 writeableFinishedImpl w

foreign import writeableFinishedImpl :: forall r. EffectFn1 (Writable r) (Boolean)

writeableHighWaterMark :: forall r. Writable r -> Effect Number
writeableHighWaterMark w = runEffectFn1 writeableHighWaterMarkImpl w

foreign import writeableHighWaterMarkImpl :: forall r. EffectFn1 (Writable r) (Number)

writeableLength :: forall r. Writable r -> Effect Number
writeableLength w = runEffectFn1 writeableLengthImpl w

foreign import writeableLengthImpl :: forall r. EffectFn1 (Writable r) (Number)

writeableNeedDrain :: forall r. Writable r -> Effect Boolean
writeableNeedDrain w = runEffectFn1 writeableNeedDrainImpl w

foreign import writeableNeedDrainImpl :: forall r. EffectFn1 (Writable r) (Boolean)

write :: forall r. Writable r -> Buffer -> Effect Boolean
write w b = runEffectFn2 writeImpl w b

write_ :: forall r. Writable r -> Buffer -> Effect Unit
write_ w b = void $ write w b

foreign import writeImpl :: forall r a. EffectFn2 (Writable r) (Buffer) (a)

writeCb :: forall r. Writable r -> Buffer -> (Maybe Error -> Effect Unit) -> Effect Boolean
writeCb w b cb = runEffectFn3 writeCbImpl w b $ mkEffectFn1 \err -> cb (toMaybe err)

writeCb_ :: forall r. Writable r -> Buffer -> (Maybe Error -> Effect Unit) -> Effect Unit
writeCb_ w b cb = void $ writeCb w b cb

foreign import writeCbImpl :: forall r a. EffectFn3 (Writable r) (Buffer) (EffectFn1 (Nullable Error) Unit) (a)

writeString :: forall r. Writable r -> Encoding -> String -> Effect Boolean
writeString w enc str = runEffectFn3 writeStringImpl w str (encodingToNode enc)

writeString_ :: forall r. Writable r -> Encoding -> String -> Effect Unit
writeString_ w enc str = void $ writeString w enc str

foreign import writeStringImpl :: forall r a. EffectFn3 (Writable r) (String) (String) (a)

writeStringCb :: forall r. Writable r -> Encoding -> String -> (Maybe Error -> Effect Unit) -> Effect Boolean
writeStringCb w enc str cb = runEffectFn4 writeStringCbImpl w str (encodingToNode enc) $ mkEffectFn1 \err -> cb (toMaybe err)

writeStringCb_ :: forall r. Writable r -> Encoding -> String -> (Maybe Error -> Effect Unit) -> Effect Unit
writeStringCb_ w enc str cb = void $ writeStringCb w enc str cb

foreign import writeStringCbImpl :: forall r a. EffectFn4 (Writable r) (String) (String) (EffectFn1 (Nullable Error) Unit) (a)

-- | Force buffering of writes.
cork :: forall r. Writable r -> Effect Unit
cork s = runEffectFn1 corkImpl s

foreign import corkImpl :: forall r. EffectFn1 (Writable r) (Unit)

-- | Flush buffered data.
uncork :: forall r. Writable r -> Effect Unit
uncork w = runEffectFn1 uncorkImpl w

foreign import uncorkImpl :: forall r. EffectFn1 (Writable r) (Unit)

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

-- | End writing data to the stream.
end :: forall r. Writable r -> (Maybe Error -> Effect Unit) -> Effect Unit
end w cb = runEffectFn2 endCbImpl w $ mkEffectFn1 \err -> cb (toMaybe err)

foreign import endCbImpl :: forall r. EffectFn2 (Writable r) (EffectFn1 (Nullable Error) Unit) (Unit)

end_ :: forall r. Writable r -> Effect Unit
end_ w = runEffectFn1 endImpl w

foreign import endImpl :: forall r. EffectFn1 (Writable r) (Unit)

destroy :: forall r. Stream r -> Effect Unit
destroy w = runEffectFn1 destroyImpl w

foreign import destroyImpl :: forall r. EffectFn1 (Stream r) (Unit)

destroy' :: forall r. Stream r -> Error -> Effect Unit
destroy' w e = runEffectFn2 destroyErrorImpl w e

foreign import destroyErrorImpl :: forall r. EffectFn2 (Stream r) (Error) Unit

closed :: forall r. Stream r -> Effect Boolean
closed w = runEffectFn1 closedImpl w

foreign import closedImpl :: forall r. EffectFn1 (Stream r) (Boolean)

destroyed :: forall r. Stream r -> Effect Boolean
destroyed w = runEffectFn1 destroyedImpl w

foreign import destroyedImpl :: forall r. EffectFn1 (Stream r) (Boolean)
