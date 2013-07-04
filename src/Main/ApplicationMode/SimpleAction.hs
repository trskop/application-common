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
--
-- Module introduced in version @0.1.1.0@.
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

-- | Introduced in version @0.1.1.0@.
data SimpleAction
    = PrintVersion Bool
    -- | Print version information. When boolean argument is 'False' then it
    -- indicates that version information should be printed in its full form.
    -- Otherwise when it's 'True', then it should print only version number.
    | PrintHelp
    -- | Print help (version, usage and option) information.
    | Action
    -- | Do the real work.
    | OptErrors [String]
    -- | List of errors that occurred while processing command line arguments.
    deriving (Data, Show, Typeable)

-- | Arguments to data constructors are ignored. It's useful to determine in
-- which mode of operation we are.
instance Eq SimpleAction where
    PrintVersion _ == PrintVersion _ = True
    PrintHelp      == PrintHelp      = True
    Action         == Action         = True
    OptErrors _    == OptErrors _    = True
    _              == _              = False

instance Default SimpleAction where
    def = Action

instance Semigroup SimpleAction where
    x <> y = case x of
        OptErrors es1 -> case y of
            OptErrors es2 -> OptErrors $ es1 ++ es2
            _ -> x
        PrintVersion _ -> case y of
            OptErrors _ -> y
            _ -> x
        PrintHelp -> case y of
            OptErrors _ -> y
            _ -> x
        Action -> y

-- | Wrapper around 'OptErrors' data constructor.
--
-- It returns 'Nothing' if list of error messages is empty and if it's not
-- empty, then it constructs 'SimpleAction', but each message is taken up to
-- first embedded new line character. This handles issue with
-- @System.Console.GetOpt.getOpt@ that includes new line character as part of
-- an error message.
--
-- Introduced in version @0.1.1.0@.
optErrors :: [String] -> Maybe SimpleAction
optErrors []   = Nothing
optErrors msgs = Just . OptErrors
    $ map (takeWhile $ isNot '\r' <&&> isNot '\n') msgs

-- }}} SimpleAction -----------------------------------------------------------
