module FOMObot.Helpers.Free
    ( liftFree
    ) where

import Control.Monad.Free (Free(Free))

liftFree :: Functor f => f a -> Free f a
liftFree action = Free (fmap return action)
