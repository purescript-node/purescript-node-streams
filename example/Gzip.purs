module Gzip where

import Prelude

import Node.Stream

import Effect
import Effect.Console


foreign import gzip :: forall eff. Eff (gzip :: GZIP | eff) (Duplex (gzip :: GZIP | eff))
foreign import stdin :: forall eff. Readable () (console :: CONSOLE | eff)
foreign import stdout :: forall eff. Writable () (console :: CONSOLE | eff)

main = do
  z <- gzip
  _ <- stdin `pipe` z
  z     `pipe` stdout
