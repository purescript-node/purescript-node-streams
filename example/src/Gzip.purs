module Gzip where

import Prelude

import Effect (Effect)
import Node.Stream (Duplex, Readable, Writable, pipe)

foreign import gzip :: Effect Duplex
foreign import fileStream :: Readable ()
foreign import stdout :: Writable ()

main :: Effect Unit
main = do
  z <- gzip
  fileStream `pipe` z
  z `pipe` stdout
