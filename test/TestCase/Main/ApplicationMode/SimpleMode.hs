-- |
-- Module:       $HEADER$
-- Description:  Tests for module Main.ApplicationMode.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @Main.ApplicationMode@.
module TestCase.Main.ApplicationMode.SimpleMode (tests)
    where

import Control.Comonad (Comonad(..))
import Data.Default.Class (Default(def))
import Data.Default.Instances.Base ()
import Data.Functor.FlipT (FlipT(..), flipmap)
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
    [ testCase "def :: SimpleMode () ()"
        $ (def :: (), def :: ()) @=? fromSimpleMode def
    , testCase "def :: SimpleMode [Int] [Int]"
        $ (def :: [Int], def :: [Int]) @=? fromSimpleMode def
    ]

test_AppModeFunctorInstance :: [Test]
test_AppModeFunctorInstance =
    [ testCase "(+1) `fmap` SimpleMode ((), 41 :: Int)) = ((), 42)"
        $ ((), 42 :: Int) @=? fromSimpleMode ((+1) `fmap` SimpleMode ((), 41))
    ]

test_FlipTAppModeFunctorInstance :: [Test]
test_FlipTAppModeFunctorInstance =
    [ testCase "(+1) `flipmap` SimpleMode (1 :: Int, ())) = (2, ())"
        $ (2 :: Int, ()) @=? fromSimpleMode ((+1) `flipmap` SimpleMode (1, ()))
    ]

test_AppModeApplicativeInstance :: [Test]
test_AppModeApplicativeInstance = []

test_AppModeComonadInstance :: [Test]
test_AppModeComonadInstance =
    [ testCase "extract (SimpleMode (42 :: Int, ())) = ()"
        $ () @=? extract (SimpleMode (42 :: Int, ()))
    , testCase "extract (SimpleMode ((), 42 :: Int)) = 42"
        $ 42 @=? extract (SimpleMode ((), 42 :: Int))
    ]

test_FlipTAppModeComonadInstance :: [Test]
test_FlipTAppModeComonadInstance =
    [ testCase "extract (FlipT (SimpleMode (42 :: Int, ()))) = 42"
        $ 42 @=? extract (FlipT (SimpleMode (42 :: Int, ())))
    , testCase "extract (FlipT (SimpleMode ((), 42 :: Int))) = ()"
        $ () @=? extract (FlipT (SimpleMode ((), 42 :: Int)))
    ]

test_AppModeMonadInstance :: [Test]
test_AppModeMonadInstance = []
