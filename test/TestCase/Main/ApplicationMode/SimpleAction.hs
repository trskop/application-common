-- |
-- Module:       $HEADER$
-- Description:  Tests for module Main.ApplicationMode.SimpleAction.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @Main.ApplicationMode.SimpleAction@.

module TestCase.Main.ApplicationMode.SimpleAction (tests)
    where

import Data.Default.Class (Default(def))
import Data.Semigroup (Semigroup((<>)))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool)

import Main.ApplicationMode.SimpleAction


tests :: [Test]
tests =
    [ testCase "instance Default SimpleAction"
        . assertBool "Expected default value to be Action." $ isAction def
    , testGroup "instance Eq SimpleAction"
        $ test_EqSimpleActionInstance
    , testGroup "instance Semigroup SimpleAction"
        $ test_SemigroupSimpleActionInstance
    , testGroup "optErrors"
        $ test_optErrors
    ]
  where
    isAction Action = True
    isAction _      = False

test_EqSimpleActionInstance :: [Test]
test_EqSimpleActionInstance =
    [ testEqual Action Action
    , testEqual PrintHelp PrintHelp
    , testEqual (PrintVersion True) (PrintVersion True)
    , testEqual (PrintVersion True) (PrintVersion False)
    , testEqual (OptErrors []) (OptErrors ["foo"])
    , testInequal Action PrintHelp
    , testInequal PrintHelp (PrintVersion True)
    , testInequal (OptErrors []) Action
    , testInequal Action (OptErrors [])
    ]
  where
    testEqual x y = testCase (show x ++ " == " ++ show y)
        . assertBool (msg x y "equal") $ x == y
    testInequal x y = testCase (show x ++ " /= " ++ show y)
        . assertBool (msg x y "inequal") $ x /= y
    msg x y str = concat ["Expected ", show x, " to be ", str, " to ", show y]

test_SemigroupSimpleActionInstance :: [Test]
test_SemigroupSimpleActionInstance =
    [ testCase "OptErrors [\"foo\"] <> PrintVersion False"
        . expectOptErrors ["foo"] $ OptErrors ["foo"] <> PrintVersion False
    , testCase "OptErrors [\"foo\"] <> PrintHelp"
        . expectOptErrors ["foo"] $ OptErrors ["foo"] <> PrintHelp
    , testCase "OptErrors [\"foo\"] <> Action"
        . expectOptErrors ["foo"] $ OptErrors ["foo"] <> Action
    , testCase "OptErrors [\"foo\"] <> OptErrors [\"bar\"]"
        . expectOptErrors ["foo", "bar"]
        $ OptErrors ["foo"] <> OptErrors ["bar"]
    , testCase "Action <> OptErrors [\"foo\"]"
        . expectOptErrors ["foo"] $ Action <> OptErrors ["foo"]
    , testCase "PrintHelp <> OptErrors [\"foo\"]"
        . expectOptErrors ["foo"] $ PrintHelp <> OptErrors ["foo"]
    , testCase "PrintVersion True <> OptErrors [\"foo\"]"
        . expectOptErrors ["foo"] $ PrintVersion True <> OptErrors ["foo"]

    , testCase "PrintHelp <> PrintVersion True"
        . expectPrintHelp () $ PrintHelp <> PrintVersion True
    , testCase "PrintVersion False <> PrintHelp"
        . expectPrintVersion False $ PrintVersion False <> PrintHelp

    , testCase "Action <> PrintHelp"
        . expectPrintHelp () $ Action <> PrintHelp
    , testCase "PrintHelp <> Action"
        . expectPrintHelp () $ PrintHelp <> Action

    , testCase "Action <> PrintVersion False"
        . expectPrintVersion False $ Action <> PrintVersion False
    , testCase "PrintVersion True <> Action"
        . expectPrintVersion True $ PrintVersion True <> Action

    , testCase "Action <> Action"
        . expectAction () $ Action <> Action
    ]
  where
    expect g f param x = assertBool
        ("Expected " ++ show (g param) ++ " but got " ++ show x)
        $ f param x

    expectOptErrors = expect OptErrors $ \ msgs x -> case x of
        OptErrors msgs' -> msgs == msgs'
        _ -> False

    expectAction = expect (const Action) $ \ () x -> case x of
        Action -> True
        _ -> False

    expectPrintHelp = expect (const PrintHelp) $ \ () x -> case x of
        PrintHelp -> True
        _ -> False

    expectPrintVersion = expect PrintVersion $ \ b x -> case x of
        PrintVersion b' -> b == b'
        _ -> False

test_optErrors :: [Test]
test_optErrors = []
