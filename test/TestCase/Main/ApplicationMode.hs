-- |
-- Module:       $HEADER$
-- Description:  Tests for module Main.ApplicationMode.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @Main.ApplicationMode@.
module TestCase.Main.ApplicationMode (tests)
    where

import Data.Default.Class (def)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))

import Main.ApplicationMode


tests :: [Test]
tests =
    [ testGroup "instance (Default a, Default c) => Default (AppMode a c)"
        test_AppModeDefaultInstance
    , testGroup "instance Functor (AppMode a)"
        test_AppModeFunctorInstance
    , testGroup "instance Functor (FlipT AppMode c)"
        test_FlipTAppModeFunctorInstance
    , testGroup "instance (Default a, Semigroup a) => Applicative (AppMode a)"
        test_AppModeApplicativeInstance
    , testGroup "instance Comonad (AppMode a)"
        test_AppModeComonadInstance
    , testGroup "instance Comonad (FlipT AppMode c)"
        test_FlipTAppModeComonadInstance
    , testGroup "instance (Default a, Semigroup a) => Monad (AppMode a)"
        test_AppModeMonadInstance
    ]

test_AppModeDefaultInstance :: [Test]
test_AppModeDefaultInstance =
    [ testCase "def :: AppMode () ()"
        ((), ()) @=? fromAppMode def
    ]

test_AppModeFunctorInstance :: [Test]
test_AppModeFunctorInstance = []

test_FlipTAppModeFunctorInstance :: [Test]
test_FlipTAppModeFunctorInstance = []

test_AppModeApplicativeInstance :: [Test]
test_AppModeApplicativeInstance = []

test_AppModeComonadInstance :: [Test]
test_AppModeComonadInstance = []

test_FlipTAppModeComonadInstance :: [Test]
test_FlipTAppModeComonadInstance = []

test_AppModeMonadInstance :: [Test]
test_AppModeMonadInstance = []
