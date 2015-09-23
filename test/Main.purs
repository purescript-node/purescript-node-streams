module Test.Main where

import Prelude

import Node.Stream

import Control.Monad.Eff
import Control.Monad.Eff.Console

foreign import data GZIP :: !

foreign import stdin :: forall eff. Readable () (console :: CONSOLE | eff) String

foreign import stdout :: forall eff. Writable () (console :: CONSOLE | eff) String

foreign import gzip :: forall eff a. Eff (gzip :: GZIP | eff) (Duplex (gzip :: GZIP | eff) a)

main = do
  z <- gzip
  stdin `pipe` z 
  z     `pipe` stdout
