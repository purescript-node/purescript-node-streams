module Gzip where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Stream (Duplex, Readable, Writable, onEnd, pipe)

foreign import data GZIP :: Effect

foreign import gzip :: forall eff. Eff (gzip :: GZIP | eff) (Duplex (gzip :: GZIP | eff))
foreign import fileStream :: forall eff. Readable () (console :: CONSOLE | eff)
foreign import stdout :: forall eff. Writable () (console :: CONSOLE | eff)

main :: forall eff. Eff (gzip :: GZIP, console :: CONSOLE | eff) Unit
main = do
  z <- gzip
  _ <- fileStream `pipe` z
  _ <- z `pipe` stdout
  void <<< onEnd fileStream $
    log "Done reading file, gzip output below"
