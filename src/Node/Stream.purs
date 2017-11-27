-- | This module provides a low-level wrapper for the Node Stream API.

module Node.Stream
  ( Stream()
  , Read()
  , Readable()
  , Write()
  , Writable()
  , Duplex()
  , onData
  , onDataString
  , onDataEither
  , setEncoding
  , onReadable
  , onEnd
  , onFinish
  , onClose
  , onError
  , resume
  , pause
  , isPaused
  , pipe
  , unpipe
  , unpipeAll
  , read
  , readString
  , readEither
  , write
  , writeString
  , cork
  , uncork
  , setDefaultEncoding
  , end
  , destroy
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (throw, EXCEPTION(), Error())
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Node.Buffer (Buffer())
import Node.Buffer as Buffer
import Node.Encoding (Encoding)

-- | A stream.
-- |
-- | The type arguments track, in order:
-- |
-- | - Whether reading and/or writing from/to the stream are allowed.
-- | - Effects associated with reading/writing from/to this stream.
foreign import data Stream :: # Type -> # Effect -> Type

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
  :: forall w eff
   . Readable w (exception :: EXCEPTION | eff)
  -> (Buffer -> Eff (exception :: EXCEPTION | eff) Unit)
  -> Eff (exception :: EXCEPTION | eff) Unit
onData r cb =
  onDataEither r (cb <=< fromEither)
  where
  fromEither x =
    case x of
      Left _  ->
        throw "Stream encoding should not be set"
      Right buf ->
        pure buf

read
  :: forall w eff
   . Readable w (exception :: EXCEPTION | eff)
   -> Maybe Int
   -> Eff (exception :: EXCEPTION | eff) (Maybe Buffer)
read r size = do
  v <- readEither r size
  case v of
    Nothing        -> pure Nothing
    Just (Left _)  -> throw "Stream encoding should not be set"
    Just (Right b) -> pure (Just b)

readString
  :: forall w eff
   . Readable w (exception :: EXCEPTION | eff)
  -> Maybe Int
  -> Encoding
  -> Eff (exception :: EXCEPTION | eff) (Maybe String)
readString r size enc = do
  v <- readEither r size
  case v of
       Nothing          -> pure Nothing
       Just (Left _)    -> throw "Stream encoding should not be set"
       Just (Right buf) -> Just <$> (unsafeCoerceEff $ Buffer.toString enc buf)

readEither
  :: forall w eff
   . Readable w eff
  -> Maybe Int
  -> Eff eff (Maybe (Either String Buffer))
readEither r size = readImpl readChunk Nothing Just r (fromMaybe undefined size)

foreign import readImpl
  :: forall r eff
   . (Chunk -> Either String Buffer)
  -> (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Readable r eff
  -> Int
  -> Eff eff (Maybe (Either String Buffer))

-- | Listen for `data` events, returning data in a String, which will be
-- | decoded using the given encoding. Note that this will fail if `setEncoding`
-- | has been called on the stream.
onDataString
  :: forall w eff
   . Readable w (exception :: EXCEPTION | eff)
  -> Encoding
  -> (String -> Eff (exception :: EXCEPTION | eff) Unit)
  -> Eff (exception :: EXCEPTION | eff) Unit
onDataString r enc cb = onData r (cb <=< unsafeCoerceEff <<< Buffer.toString enc)

-- | Listen for `data` events, returning data in an `Either String Buffer`. This
-- | function is provided for the (hopefully rare) case that `setEncoding` has
-- | been called on the stream.
onDataEither
  :: forall r eff
   . Readable r (exception :: EXCEPTION | eff)
  -> (Either String Buffer -> Eff (exception :: EXCEPTION | eff) Unit)
  -> Eff (exception :: EXCEPTION | eff) Unit
onDataEither r cb = onDataEitherImpl readChunk r cb

foreign import onDataEitherImpl
  :: forall r eff
   . (Chunk -> Either String Buffer)
  -> Readable r eff
  -> (Either String Buffer -> Eff eff Unit)
  -> Eff eff Unit

foreign import setEncodingImpl
  :: forall w eff
   . Readable w eff
  -> String
  -> Eff eff Unit

-- | Set the encoding used to read chunks as strings from the stream. This
-- | function may be useful when you are passing a readable stream to some other
-- | JavaScript library, which already expects an encoding to be set.
-- |
-- | Where possible, you should try to use `onDataString` instead of this
-- | function.
setEncoding
  :: forall w eff
   . Readable w eff
  -> Encoding
  -> Eff eff Unit
setEncoding r enc = setEncodingImpl r (show enc)

-- | Listen for `readable` events.
foreign import onReadable
  :: forall w eff
   . Readable w eff
  -> Eff eff Unit
  -> Eff eff Unit

-- | Listen for `end` events.
foreign import onEnd
  :: forall w eff
   . Readable w eff
  -> Eff eff Unit
  -> Eff eff Unit

-- | Listen for `finish` events.
foreign import onFinish
  :: forall w eff
   . Writable w eff
  -> Eff eff Unit
  -> Eff eff Unit

-- | Listen for `close` events.
foreign import onClose
  :: forall w eff
   . Stream w eff
  -> Eff eff Unit
  -> Eff eff Unit

-- | Listen for `error` events.
foreign import onError
  :: forall w eff
   . Stream w eff
  -> (Error -> Eff eff Unit)
  -> Eff eff Unit

-- | Resume reading from the stream.
foreign import resume :: forall w eff. Readable w eff -> Eff eff Unit

-- | Pause reading from the stream.
foreign import pause :: forall w eff. Readable w eff -> Eff eff Unit

-- | Check whether or not a stream is paused for reading.
foreign import isPaused :: forall w eff. Readable w eff -> Eff eff Boolean

-- | Read chunks from a readable stream and write them to a writable stream.
foreign import pipe
  :: forall r w eff
   . Readable w eff
  -> Writable r eff
  -> Eff eff (Writable r eff)

-- | Detach a Writable stream previously attached using `pipe`.
foreign import unpipe
  :: forall r w eff
   . Readable w eff
  -> Writable r eff
  -> Eff eff Unit

-- | Detach all Writable streams previously attached using `pipe`.
foreign import unpipeAll
  :: forall w eff
   . Readable w eff
  -> Eff eff Unit

-- | Write a Buffer to a writable stream.
foreign import write
  :: forall r eff
   . Writable r eff
  -> Buffer
  -> Eff eff Unit
  -> Eff eff Boolean

foreign import writeStringImpl
  :: forall r eff
   . Writable r eff
  -> String
  -> String
  -> Eff eff Unit
  -> Eff eff Boolean

-- | Write a string in the specified encoding to a writable stream.
writeString
  :: forall r eff
   . Writable r eff
  -> Encoding
  -> String
  -> Eff eff Unit
  -> Eff eff Boolean
writeString w enc = writeStringImpl w (show enc)

-- | Force buffering of writes.
foreign import cork :: forall r eff. Writable r eff -> Eff eff Unit

-- | Flush buffered data.
foreign import uncork :: forall r eff. Writable r eff -> Eff eff Unit

foreign import setDefaultEncodingImpl
  :: forall r eff
   . Writable r eff
  -> String
  -> Eff eff Unit

-- | Set the default encoding used to write strings to the stream. This function
-- | is useful when you are passing a writable stream to some other JavaScript
-- | library, which already expects a default encoding to be set. It has no
-- | effect on the behaviour of the `writeString` function (because that
-- | function ensures that the encoding is always supplied explicitly).
setDefaultEncoding
  :: forall r eff
   . Writable r eff
  -> Encoding
  -> Eff eff Unit
setDefaultEncoding r enc = setDefaultEncodingImpl r (show enc)

-- | End writing data to the stream.
foreign import end
  :: forall r eff
   . Writable r eff
  -> Eff eff Unit
  -> Eff eff Unit

-- | Destroy the stream. It will release any internal resources.
--
-- Added in node 8.0.
foreign import destroy
  :: forall r eff
   . Stream r eff
  -> Eff eff Unit

-- | Destroy the stream and emit 'error'.
--
-- Added in node 8.0.
foreign import destroyWithError
  :: forall r eff
   . Stream r eff
  -> Error
  -> Eff eff Unit
