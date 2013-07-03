{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module:       $HEADER$
-- Description:  Basic implementation of application mode.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (FlexibleInstances)
--
-- Basic implementation of application mode.
module Main.ApplicationMode.SimpleMode
    ( SimpleMode(..)
    )
    where

import Control.Applicative (Applicative(..))

import Control.Comonad (Comonad(..))
import Data.Default.Class (Default(def))
import Data.Functor.FlipT (FlipT(FlipT))
import Data.Semigroup (Semigroup(..))


-- {{{ SimpleMode -------------------------------------------------------------

newtype SimpleMode a b = SimpleMode {fromSimpleMode :: (a, b)}

instance (Default a, Default c) => Default (SimpleMode a c) where
    def = SimpleMode (def, def)

instance Functor (SimpleMode a) where
    fmap f (SimpleMode (x, y)) = SimpleMode (x, f y)

instance Functor (FlipT SimpleMode c) where
    fmap f (FlipT (SimpleMode (x, y))) = FlipT $ SimpleMode (f x, y)

instance (Default a, Semigroup a) => Applicative (SimpleMode a) where
    pure x = SimpleMode (def, x)
    SimpleMode (a, f) <*> SimpleMode (a', c) = SimpleMode (a <> a', f c)

instance Comonad (SimpleMode a) where
    extract (SimpleMode (_, c)) = c
    duplicate x@(SimpleMode (a, _)) = SimpleMode (a, x)

instance Comonad (FlipT SimpleMode c) where
    extract (FlipT (SimpleMode (a, _))) = a
    duplicate x@(FlipT (SimpleMode (_, c))) = FlipT (SimpleMode (x, c))

instance (Default a, Semigroup a) => Monad (SimpleMode a) where
    return x = SimpleMode (def, x)
    SimpleMode (a, c) >>= f = SimpleMode (a <> a', c')
      where (a', c') = fromSimpleMode $ f c

-- }}} SimpleMode -------------------------------------------------------------
