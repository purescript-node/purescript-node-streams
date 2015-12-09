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

import Control.Monad.Eff

-- | A stream.
-- |
-- | The type arguments track, in order:
-- |
-- | - Whether reading and/or writing from/to the stream are allowed.
-- | - Effects associated with reading/writing from/to this stream.
-- | - The type of chunks which will be read from/written to this stream (`String` or `Buffer`).
foreign import data Stream :: # * -> # ! -> * -> *

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

foreign import setEncodingImpl :: forall w eff. Readable w eff String -> String -> Eff eff Unit

-- | Set the encoding used to read chunks from the stream.
setEncoding :: forall w eff. Readable w eff String -> Encoding -> Eff eff Unit
setEncoding r enc = setEncodingImpl r (show enc)

-- | Listen for `data` events.
foreign import onData :: forall w eff a. Readable w eff a -> (a -> Eff eff Unit) -> Eff eff Unit

-- | Listen for `end` events.
foreign import onEnd :: forall w eff a. Readable w eff a -> Eff eff Unit -> Eff eff Unit

-- | Listen for `close` events.
foreign import onClose :: forall w eff a. Readable w eff a -> Eff eff Unit -> Eff eff Unit

-- | Listen for `error` events.
foreign import onError :: forall w eff a. Readable w eff a -> Eff eff Unit -> Eff eff Unit

-- | Resume reading from the stream.
foreign import resume :: forall w eff a. Readable w eff a -> Eff eff Unit

-- | Pause reading from the stream.
foreign import pause :: forall w eff a. Readable w eff a -> Eff eff Unit

-- | Check whether or not a stream is paused for reading.
foreign import isPaused :: forall w eff a. Readable w eff a -> Eff eff Boolean

-- | Read chunks from a readable stream and write them to a writable stream.
foreign import pipe :: forall r w eff a. Readable w eff a -> Writable r eff a -> Eff eff (Writable r eff a)

-- | Write a chunk to a writable stream.
foreign import write :: forall r eff a. Writable r eff String -> a -> Eff eff Unit -> Eff eff Boolean

foreign import writeStringImpl :: forall r eff. Writable r eff String -> String -> String -> Eff eff Unit -> Eff eff Boolean

-- | Write a string in the specified encoding to a writable stream.
writeString :: forall r eff. Writable r eff String -> Encoding -> String -> Eff eff Unit -> Eff eff Boolean
writeString w enc = writeStringImpl w (show enc)

-- | Force buffering of writes.
foreign import cork :: forall r eff a. Writable r eff a -> Eff eff Unit

-- | Flush buffered data.
foreign import uncork :: forall r eff a. Writable r eff a -> Eff eff Unit

foreign import setDefaultEncodingImpl :: forall r eff. Writable r eff String -> String -> Eff eff Unit

-- | Set the default encoding used to write chunks to the stream.
setDefaultEncoding :: forall r eff. Writable r eff String -> Encoding -> Eff eff Unit
setDefaultEncoding r enc = setDefaultEncodingImpl r (show enc)

-- | End writing data to the stream.
foreign import end :: forall r eff a. Writable r eff a -> Eff eff Unit -> Eff eff Unit
