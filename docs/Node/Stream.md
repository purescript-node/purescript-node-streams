## Module Node.Stream

This module provides a low-level wrapper for the Node Stream API.

#### `Stream`

``` purescript
data Stream :: # * -> # ! -> *
```

A stream.

The type arguments track, in order:

- Whether reading and/or writing from/to the stream are allowed.
- Effects associated with reading/writing from/to this stream.

#### `Read`

``` purescript
data Read
```

A phantom type associated with _readable streams_.

#### `Readable`

``` purescript
type Readable r = Stream (read :: Read | r)
```

A readable stream.

#### `Write`

``` purescript
data Write
```

A phantom type associated with _writable streams_.

#### `Writable`

``` purescript
type Writable r = Stream (write :: Write | r)
```

A writable stream.

#### `Duplex`

``` purescript
type Duplex = Stream (read :: Read, write :: Write)
```

A duplex (readable _and_ writable stream)

#### `onData`

``` purescript
onData :: forall w eff. Readable w (err :: EXCEPTION | eff) -> (Buffer -> Eff (err :: EXCEPTION | eff) Unit) -> Eff (err :: EXCEPTION | eff) Unit
```

Listen for `data` events, returning data in a Buffer. Note that this will fail
if `setEncoding` has been called on the stream.

#### `onDataString`

``` purescript
onDataString :: forall w eff. Readable w (err :: EXCEPTION | eff) -> Encoding -> (String -> Eff (err :: EXCEPTION | eff) Unit) -> Eff (err :: EXCEPTION | eff) Unit
```

Listen for `data` events, returning data in a String, which will be
decoded using the given encoding. Note that this will fail if `setEncoding`
has been called on the stream.

#### `onDataEither`

``` purescript
onDataEither :: forall w eff. Readable w eff -> (Either String Buffer -> Eff eff Unit) -> Eff eff Unit
```

Listen for `data` events, returning data in an `Either String Buffer`. This
function is provided for the (hopefully rare) case that `setEncoding` has
been called on the stream.

#### `setEncoding`

``` purescript
setEncoding :: forall w eff. Readable w eff -> Encoding -> Eff eff Unit
```

Set the encoding used to read chunks as strings from the stream. This
function may be useful when you are passing a readable stream to some other
JavaScript library, which already expects an encoding to be set.

Where possible, you should try to use `onDataString` instead of this
function.

#### `onEnd`

``` purescript
onEnd :: forall w eff. Readable w eff -> Eff eff Unit -> Eff eff Unit
```

Listen for `end` events.

#### `onClose`

``` purescript
onClose :: forall w eff. Readable w eff -> Eff eff Unit -> Eff eff Unit
```

Listen for `close` events.

#### `onError`

``` purescript
onError :: forall w eff. Readable w eff -> Eff eff Unit -> Eff eff Unit
```

Listen for `error` events.

#### `resume`

``` purescript
resume :: forall w eff. Readable w eff -> Eff eff Unit
```

Resume reading from the stream.

#### `pause`

``` purescript
pause :: forall w eff. Readable w eff -> Eff eff Unit
```

Pause reading from the stream.

#### `isPaused`

``` purescript
isPaused :: forall w eff. Readable w eff -> Eff eff Boolean
```

Check whether or not a stream is paused for reading.

#### `pipe`

``` purescript
pipe :: forall r w eff. Readable w eff -> Writable r eff -> Eff eff (Writable r eff)
```

Read chunks from a readable stream and write them to a writable stream.

#### `write`

``` purescript
write :: forall r eff. Writable r eff -> Buffer -> Eff eff Unit -> Eff eff Boolean
```

Write a Buffer to a writable stream.

#### `writeString`

``` purescript
writeString :: forall r eff. Writable r eff -> Encoding -> String -> Eff eff Unit -> Eff eff Boolean
```

Write a string in the specified encoding to a writable stream.

#### `cork`

``` purescript
cork :: forall r eff. Writable r eff -> Eff eff Unit
```

Force buffering of writes.

#### `uncork`

``` purescript
uncork :: forall r eff. Writable r eff -> Eff eff Unit
```

Flush buffered data.

#### `setDefaultEncoding`

``` purescript
setDefaultEncoding :: forall r eff. Writable r eff -> Encoding -> Eff eff Unit
```

Set the default encoding used to write strings to the stream. This function
is useful when you are passing a writable stream to some other JavaScript
library, which already expects a default encoding to be set. It has no
effect on the behaviour of the `writeString` function (because that
function ensures that the encoding is always supplied explicitly).

#### `end`

``` purescript
end :: forall r eff. Writable r eff -> Eff eff Unit -> Eff eff Unit
```

End writing data to the stream.


