module Gzip where

import Prelude

import Node.Stream
import Node.Stream.StdIO

import Control.Monad.Eff
import Control.Monad.Eff.Console

foreign import data GZIP :: !

foreign import gzip :: forall eff. Eff (gzip :: GZIP | eff) (Duplex (gzip :: GZIP | eff))

foreign import

main = do
  z <- gzip
  stdin `pipe` z
  z     `pipe` stdout
