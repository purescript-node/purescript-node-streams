-- | This module provides a low-level wrapper for the Node Stream API.

module Node.Stream
  ( Stream()
  , Read()
  , Readable()
  , Write()
  , Writable()
  , Duplex()
  , setEncoding
  , onData
  , onDataString
  , onEnd
  , onClose
  , onError
  , resume
  , pause
  , isPaused
  , pipe
  , write
  , writeString
  , cork
  , uncork
  , setDefaultEncoding
  , end
  ) where

import Prelude

import Node.Encoding
import Node.Buffer (Buffer())
import Node.Buffer as Buffer

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)

-- | A stream.
-- |
-- | The type arguments track, in order:
-- |
-- | - Whether reading and/or writing from/to the stream are allowed.
-- | - Effects associated with reading/writing from/to this stream.
foreign import data Stream :: # * -> # ! -> *

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

foreign import setEncodingImpl :: forall w eff. Readable w eff -> String -> Eff eff Unit

-- | Set the encoding used to read chunks as strings from the stream. This
-- | function is useful when you are passing a readable stream to some other
-- | JavaScript library, which already expects an encoding to be set. It
-- | has no effect on the behaviour of the `onDataString` function (because
-- | that function ensures that the encoding is always supplied explicitly).
setEncoding :: forall w eff. Readable w eff -> Encoding -> Eff eff Unit
setEncoding r enc = setEncodingImpl r (show enc)

foreign import onDataImpl :: forall w eff a. Readable w eff -> (a -> Eff eff Unit) -> Eff eff Unit

-- | Listen for `data` events, returning data in a Buffer.
onData :: forall w eff. Readable w eff -> (Buffer -> Eff eff Unit) -> Eff eff Unit
onData = onDataImpl

-- | Listen for `data` events, returning data in a String, which will be
-- | decoded using the given encoding.
onDataString :: forall w eff. Readable w eff -> Encoding -> (String -> Eff eff Unit) -> Eff eff Unit
onDataString r enc cb = onData r $ \buf -> do
  str <- unsafeInterleaveEff (Buffer.toString enc buf)
  cb str

-- | Listen for `end` events.
foreign import onEnd :: forall w eff. Readable w eff -> Eff eff Unit -> Eff eff Unit

-- | Listen for `close` events.
foreign import onClose :: forall w eff. Readable w eff -> Eff eff Unit -> Eff eff Unit

-- | Listen for `error` events.
foreign import onError :: forall w eff. Readable w eff -> Eff eff Unit -> Eff eff Unit

-- | Resume reading from the stream.
foreign import resume :: forall w eff. Readable w eff -> Eff eff Unit

-- | Pause reading from the stream.
foreign import pause :: forall w eff. Readable w eff -> Eff eff Unit

-- | Check whether or not a stream is paused for reading.
foreign import isPaused :: forall w eff. Readable w eff -> Eff eff Boolean

-- | Read chunks from a readable stream and write them to a writable stream.
foreign import pipe :: forall r w eff. Readable w eff -> Writable r eff -> Eff eff (Writable r eff)

-- | Write a Buffer to a writable stream.
foreign import write :: forall r eff. Writable r eff -> Buffer -> Eff eff Unit -> Eff eff Boolean

foreign import writeStringImpl :: forall r eff. Writable r eff -> String -> String -> Eff eff Unit -> Eff eff Boolean

-- | Write a string in the specified encoding to a writable stream.
writeString :: forall r eff. Writable r eff -> Encoding -> String -> Eff eff Unit -> Eff eff Boolean
writeString w enc = writeStringImpl w (show enc)

-- | Force buffering of writes.
foreign import cork :: forall r eff. Writable r eff -> Eff eff Unit

-- | Flush buffered data.
foreign import uncork :: forall r eff. Writable r eff -> Eff eff Unit

foreign import setDefaultEncodingImpl :: forall r eff. Writable r eff -> String -> Eff eff Unit

-- | Set the default encoding used to write strings to the stream. This function
-- | is useful when you are passing a writable stream to some other JavaScript
-- | library, which already expects a default encoding to be set. It has no
-- | effect on the behaviour of the `writeString` function (because that
-- | function ensures that the encoding is always supplied explicitly).
setDefaultEncoding :: forall r eff. Writable r eff -> Encoding -> Eff eff Unit
setDefaultEncoding r enc = setDefaultEncodingImpl r (show enc)

-- | End writing data to the stream.
foreign import end :: forall r eff. Writable r eff -> Eff eff Unit -> Eff eff Unit
