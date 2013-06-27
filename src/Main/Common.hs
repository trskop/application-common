-- |
-- Module:       $HEADER$
-- Description:  Commonly used functions for creating command line tools.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Commonly used functions for creating command line tools.
module Main.Common
    ( printHelp
    , printVersion
    )
    where

import Control.Monad (unless)
import Data.Version (Version, showVersion)
import System.IO (Handle, hPutStr, hPutStrLn)


-- | Print usage information in common format:
--
-- > <version-info>
-- > <eol>
-- > "Usage:" <eol>
-- > <eol>
-- > (<space>{4} <program-name> <space> <usage> <eol>)*
-- > <help>
-- > (<footer-line> <eol>)*
printHelp
    :: String
    -- ^ Program name.
    -> Version
    -- ^ Program version.
    -> Handle
    -- ^ 'Handle' to which help information is printed.
    -> [String]
    -- ^ Usage information.
    -> (String -> String)
    -- ^ Construct help information, parametrized with program name.
    -> [String]
    -- ^ Footer lines. Printed right after help information.
    -> IO ()
printHelp progName version h usage mkHelp footer = do
    printVersion progName [] version h False
    hPutStr h $ unlines ["", "Usage:", ""]
    hPutStrLn h . unlines $ map (("   " ++ progName ++ " ") ++) usage
    hPutStr h $ mkHelp progName
    hPutStr h $ unlines footer

-- | Print version information in common format:
--
-- > <program-name> <space> <version> <eol>
-- > (<footer-line> <eol>)*
--
-- or just:
--
-- > <version> <eol>
printVersion
    :: String
    -- ^ Program name.
    -> [String]
    -> Version
    -- ^ Program version.
    -> Handle
    -- ^ 'Handle' to which version information is printed.
    -> Bool
    -- ^ If 'False' then program name is printed along with version number.
    -- When 'True' then only version number is printed.
    -> IO ()
printVersion progName footer version h numericOnly = do
    hPutStrLn h . unwords $ withProgName [showVersion version]
    unless numericOnly . hPutStr h $ unlines footer
  where
    withProgName = if numericOnly then id else (progName :)
