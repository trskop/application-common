-- |
-- Module:       $HEADER$
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  non-portable (depends on non-portable module)
--
-- All test cases aggregated and exported as @tests :: ['Test']@.
module TestCase (tests)
    where

import Test.Framework (Test, testGroup)

import qualified TestCase.Main.ApplicationMode.SimpleMode as SimpleMode (tests)


tests :: [Test]
tests =
    [ testGroup "Main.SimpleMode" SimpleMode.tests
    ]
