{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module:       Main
-- Description:  Hello world example with help and version information.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (MultiParamTypeClasses)
--
-- Hello world example with help and version information.
module Main (main)
    where

import Control.Applicative ((<$>))
import Data.Char (toUpper)
import Data.Monoid (Endo(..), Monoid(..))
import Data.Version (showVersion)
import System.Console.GetOpt
    ( ArgDescr(NoArg)
    , ArgOrder(Permute)
    , OptDescr(Option)
    , getOpt
    )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (Handle, hPutStr, hPutStrLn, stdout, stderr)

import Data.Default.Class (Default(def))
import Data.Semigroup (Semigroup(..))
import Main.ApplicationMode
    ( ApplicationMode(..)
    , AppMode(..)
    , changeAction
    , runApplication
    , updateConfiguration
    )
import System.Console.GetOpt.UsageInfo (renderUsageInfo)

import Paths_application_common_examples (version)


type HelloWorldMode = AppMode Action Config

data Action
    = OptErrors [String]
    | PrintHelp
    | PrintVersion
    | DoHelloWorld

instance Default Action where
    def = DoHelloWorld

instance Semigroup Action where
    OptErrors msgs  <> OptErrors msgs' = OptErrors $ msgs ++ msgs'
    x@(OptErrors _) <> _               = x
    _               <> x@(OptErrors _) = x
    x               <> DoHelloWorld    = x
    _               <> x               = x

data Config = Config {allUpperCase :: Bool, outHandle :: Handle}

instance Default Config where
    def = Config False stdout

instance ApplicationMode AppMode Action Config where
    optErrors [] = mempty
    optErrors msgs = changeAction (OptErrors $ map (takeWhile notEol) msgs)
        `mappend` updateConfiguration (\ c -> c{outHandle = stderr})
      where
        notEol ch = ch /= '\r' && ch /= '\n'

options :: [OptDescr (Endo HelloWorldMode)]
options =
    [ Option "u" ["upper-case"]
        (NoArg . updateConfiguration $ \ c -> c{allUpperCase = True})
        "Print only upper case letters."
    , Option "h" ["help"]
        (NoArg $ changeAction PrintHelp)
        "Print help information and exit."
    , Option "V" ["version"]
        (NoArg $ changeAction PrintVersion)
        "Print version information and exit."
    ]

main :: IO ()
main = do
    endos <- processOpts . getOpt Permute options <$> getArgs
    runApplication endos (\ _ _ -> return Nothing) $ \ a c ->
        let handle = outHandle c
        in case a of
            OptErrors msgs -> do
                hPutStrLn handle . unlines
                    $ "Error(s) occurred while processing arguments:"
                    : map ("  " ++) msgs
                printHelp handle
                exitFailure
            PrintHelp -> printHelp handle
            PrintVersion -> hPutStrLn handle $ showVersion version
            DoHelloWorld -> hPutStrLn handle
                $ (if allUpperCase c then map toUpper else id) "Hello World!"
  where
    processOpts (endos, nonOpts, errs) = mconcat
        $ optErrors errs
        : optErrors (map ("Not an option: " ++) nonOpts)
        : endos

    printHelp h = do
        progName <- getProgName
        hPutStr h $ unlines ["Usage:", "", "  " ++ progName ++ " [OPTIONS]"]
        renderUsageInfo "" options >>= hPutStrLn h
