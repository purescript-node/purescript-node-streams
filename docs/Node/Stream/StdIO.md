## Module Node.Stream.StdIO

The standard IO streams for the current process.

#### `stdin`

``` purescript
stdin :: forall eff. Readable () (console :: CONSOLE | eff)
```

#### `stdout`

``` purescript
stdout :: forall eff. Writable () (console :: CONSOLE | eff)
```

#### `stderr`

``` purescript
stderr :: forall eff. Writable () (console :: CONSOLE | eff)
```


