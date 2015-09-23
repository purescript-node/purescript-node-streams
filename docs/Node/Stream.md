## Module Node.Stream

This module provides a low-level wrapper for the Node Stream API.

#### `Stream`

``` purescript
data Stream :: # * -> # ! -> * -> *
```

A stream.

The type arguments track, in order:

- Whether reading and/or writing from/to the stream are allowed.
- Effects associated with reading/writing from/to this stream.
- The type of chunks which will be read from/written to this stream (`String` or `Buffer`).

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

#### `setEncoding`

``` purescript
setEncoding :: forall w eff. Readable w eff String -> Encoding -> Eff eff Unit
```

Set the encoding used to read chunks from the stream.

#### `onData`

``` purescript
onData :: forall w eff a. Readable w eff a -> (a -> Eff eff Unit) -> Eff eff Unit
```

Listen for `data` events.

#### `onEnd`

``` purescript
onEnd :: forall w eff a. Readable w eff a -> Eff eff Unit -> Eff eff Unit
```

Listen for `end` events.

#### `onClose`

``` purescript
onClose :: forall w eff a. Readable w eff a -> Eff eff Unit -> Eff eff Unit
```

Listen for `close` events.

#### `onError`

``` purescript
onError :: forall w eff a. Readable w eff a -> Eff eff Unit -> Eff eff Unit
```

Listen for `error` events.

#### `resume`

``` purescript
resume :: forall w eff a. Readable w eff a -> Eff eff Unit
```

Resume reading from the stream.

#### `pause`

``` purescript
pause :: forall w eff a. Readable w eff a -> Eff eff Unit
```

Pause reading from the stream.

#### `isPaused`

``` purescript
isPaused :: forall w eff a. Readable w eff a -> Eff eff Boolean
```

Check whether or not a stream is paused for reading.

#### `pipe`

``` purescript
pipe :: forall r w eff a. Readable w eff a -> Writable r eff a -> Eff eff (Writable r eff a)
```

Read chunks from a readable stream and write them to a writable stream.

#### `write`

``` purescript
write :: forall r eff a. Writable r eff String -> a -> Eff eff Unit -> Eff eff Boolean
```

Write a chunk to a writable stream.

#### `writeString`

``` purescript
writeString :: forall r eff a. Writable r eff String -> Encoding -> String -> Eff eff Unit -> Eff eff Boolean
```

Write a string in the specified encoding to a writable stream.

#### `cork`

``` purescript
cork :: forall r eff a. Writable r eff a -> Eff eff Unit
```

Force buffering of writes.

#### `uncork`

``` purescript
uncork :: forall r eff a. Writable r eff a -> Eff eff Unit
```

Flush buffered data.

#### `setDefaultEncoding`

``` purescript
setDefaultEncoding :: forall r eff. Writable r eff String -> Encoding -> Eff eff Unit
```

Set the default encoding used to write chunks to the stream.

#### `end`

``` purescript
end :: forall r eff a. Writable r eff a -> Eff eff Unit -> Eff eff Unit
```

End writing data to the stream.


