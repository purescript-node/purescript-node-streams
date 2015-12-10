-- | The standard IO streams for the current process.
module Node.Stream.StdIO where

foreign import stdin  :: forall eff. Readable () (console :: CONSOLE | eff)
foreign import stdout :: forall eff. Writable () (console :: CONSOLE | eff)
foreign import stderr :: forall eff. Writable () (console :: CONSOLE | eff)
