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
module Main.ApplicationMode
    ( AppMode(..)
    , module Main.ApplicationMode.Class
    )
    where

import Control.Applicative (Applicative(..))

import Control.Comonad (Comonad(..))
import Data.Default.Class (Default(def))
import Data.Functor.FlipT (FlipT(FlipT))
import Data.Semigroup (Semigroup(..))

import Main.ApplicationMode.Class


-- {{{ AppMode ----------------------------------------------------------------

newtype AppMode a b = AppMode {fromAppMode :: (a, b)}

instance (Default a, Default c) => Default (AppMode a c) where
    def = AppMode (def, def)

instance Functor (AppMode a) where
    fmap f (AppMode (x, y)) = AppMode (x, f y)

instance Functor (FlipT AppMode c) where
    fmap f (FlipT (AppMode (x, y))) = FlipT $ AppMode (f x, y)

instance (Default a, Semigroup a) => Applicative (AppMode a) where
    pure x = AppMode (def, x)
    AppMode (a, f) <*> AppMode (a', c) = AppMode (a <> a', f c)

instance Comonad (AppMode a) where
    extract (AppMode (_, c)) = c
    duplicate x@(AppMode (a, _)) = AppMode (a, x)

instance Comonad (FlipT AppMode c) where
    extract (FlipT (AppMode (a, _))) = a
    duplicate x@(FlipT (AppMode (_, c))) = FlipT (AppMode (x, c))

instance (Default a, Semigroup a) => Monad (AppMode a) where
    return x = AppMode (def, x)
    AppMode (a, c) >>= f = AppMode (a <> a', c')
      where (a', c') = fromAppMode $ f c

-- }}} AppMode ----------------------------------------------------------------
