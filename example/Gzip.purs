module Gzip where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Stream (Duplex, Readable, Writable, onEnd, pipe)

foreign import gzip :: Effect Duplex
foreign import fileStream :: Readable ()
foreign import stdout :: Writable ()

main :: Effect Unit
main = do
  z <- gzip
  _ <- fileStream `pipe` z
  _ <- z `pipe` stdout
  void $ onEnd fileStream do
    log "Done reading file, gzip output below"
