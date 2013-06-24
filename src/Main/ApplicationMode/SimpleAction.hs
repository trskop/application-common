{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       $HEADER$
-- Description:  Basic implementation of SimpleAction suitable for simple
--               command line tools.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (DeriveDataTypeable)
--
-- Basic implementation of SimpleAction suitable for simple command line tools.
module Main.ApplicationMode.SimpleAction
    ( SimpleAction(..)
    , optErrors
    )
    where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Control.Applicative.Predicate ((<&&>), isNot)
import Data.Default.Class (Default(def))
import Data.Semigroup (Semigroup(..))


-- {{{ SimpleAction -----------------------------------------------------------

data SimpleAction
    = PrintVersion Bool
    | PrintHelp
    | Action
    | OptErrors [String]
    deriving (Data, Show, Typeable)

instance Default SimpleAction where
    def = Action

instance Semigroup SimpleAction where
    x <> y = case x of
        OptErrors es1 -> case y of
            OptErrors es2 -> OptErrors $ es1 ++ es2
            _ -> x
        PrintVersion _ -> x
        PrintHelp -> x
        Action -> y

-- | Wrapper around 'OptErrors' data constructor. It returns 'Nothing' if list
-- of error messages is empty and if it's not empty, then it constructs
-- 'SimpleAction', but each message is taken up to first embedded new line
-- character.
optErrors :: [String] -> Maybe SimpleAction
optErrors []   = Nothing
optErrors msgs = Just . OptErrors
    $ map (takeWhile $ isNot '\r' <&&> isNot '\n') msgs

-- }}} SimpleAction -----------------------------------------------------------
