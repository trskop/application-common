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
--
-- Module introduced in version @0.1.2.0@.
module Main.Common
    ( Parameters(..)
    , printHelp
    , printVersion
    , printOptErrors
    )
    where

import Control.Monad (unless, when)
import Data.Version (Version(Version), showVersion)
import System.IO (Handle, hPutStr, hPutStrLn, stdout)

import Data.Default.Class (Default(def))



-- | Data type that encapsulates functions for getting necessary information
-- from configuration.
--
-- Introduced in version @0.2.0.0@.
data Parameters a = Parameters
    { paramOutputHandle :: a -> Handle
    -- ^ Handle to which version and usage is printed.
    , paramProgName :: a -> String
    -- ^ Program name.
    , paramCommand :: a -> String
    -- ^ Most commonly output of @System.Environment.getProgName@.

    -- Version related:

    , paramVersion :: a -> Version
    -- ^ Program version number.
    , paramVersionInfoFooter :: a -> [String]
    -- ^ Lines printed right after version number.

    -- Help related:

    , paramUsage :: a -> [String]
    -- ^ Usage lines without program name.
    , paramHelpInfoFooter :: a -> [String]
    -- ^ Lines printed after help information.
    , paramIncludeVersionInfoInHelpInfo :: a -> Bool
    , paramIncludeVersionFooterInHelpInfo :: a -> Bool
    }

-- | All functions ignore their argument and produce empty/default values.
--
-- > def = Parameters
-- >     { paramOutputHandle = const stdout
-- >     , paramProgName = const ""
-- >     , paramCommand = const ""
-- >     , paramVersion = const $ Version [] []
-- >     , paramVersionInfoFooter = const []
-- >     , paramUsage = const []
-- >     , paramHelpInfoFooter = const []
-- >     , paramIncludeVersionInfoInHelpInfo = const True
-- >     , paramIncludeVersionFooterInHelpInfo = const False
-- >     }
--
-- Instance declaration introduced in version @0.2.0.0@ along with
-- 'Parameters' data type.
instance Default (Parameters a) where
    def = Parameters
        { paramOutputHandle = const stdout
        , paramProgName = const ""
        , paramCommand = const ""

        , paramVersion = const $ Version [] []
        , paramVersionInfoFooter = const []

        , paramUsage = const []
        , paramHelpInfoFooter = const []
        , paramIncludeVersionInfoInHelpInfo = const True
        , paramIncludeVersionFooterInHelpInfo = const False
        }

-- | Print usage information in common format:
--
-- > <version-info>
-- > <eol>
-- > <eol>
-- > "Usage:" <eol>
-- > <eol>
-- > (<space>{4} <program-name> <space> <usage> <eol>)*
-- > <help>
-- > (<footer-line> <eol>)*
--
-- Introduced in version @0.1.2.0@. Type signature changed in version
-- @0.2.0.0@.
printHelp
    :: Parameters c
    -> c
    -> (String -> String -> String)
    -- ^ Construct help information, parametrized with program name and
    -- command.
    -> IO ()
printHelp params cfg mkHelp = do
    when (get paramIncludeVersionInfoInHelpInfo)
        $ printVersion params' cfg False
    hPutStr handle . unlines $ emptyLinesIfPrintingVersionInfo ["Usage:", ""]
    hPutStrLn handle . unlines $ map (("  " ++ command ++ " ") ++) usage
    hPutStr handle $ mkHelp progName command
    unless (null footer)
        . hPutStr handle . unlines $ "" : "" : footer
  where
    get f = f params cfg
    progName = get paramProgName
    command = get paramCommand
    handle = get paramOutputHandle
    usage = get paramUsage
    footer = get paramHelpInfoFooter
    params' = params
        { paramVersionInfoFooter = if get paramIncludeVersionFooterInHelpInfo
            then paramVersionInfoFooter params
            else const []
        }
    emptyLinesIfPrintingVersionInfo = if get paramIncludeVersionInfoInHelpInfo
        then ("" :) . ("" :)
        else id

-- | Print version information in common format:
--
-- > <program-name> <space> <version> <eol>
-- > (<footer-line> <eol>)*
--
-- or just:
--
-- > <version> <eol>
--
-- Introduced in version @0.1.2.0@. Type signature changed in version
-- @0.2.0.0@.
printVersion
    :: Parameters c
    -- ^ Program name.
    -> c
    -> Bool
    -- ^ If 'False' then program name is printed along with version number.
    -- When 'True' then only version number is printed.
    -> IO ()
printVersion params cfg numericOnly = do
    hPutStrLn handle . unwords $ withProgName [showVersion version]
    unless numericOnly . hPutStr handle $ unlines footer
  where
    get f = f params cfg
    progName = get paramProgName
    footer = get paramVersionInfoFooter
    version = get paramVersion
    handle = get paramOutputHandle
    withProgName = if numericOnly then id else (progName :)

-- | Print option errors in this format:
--
-- > "Error(s) occurred while processing command line options:" <eol>
-- > (<space>{2} <option-error> <eol>)+
--
-- If list of errors is empty, then nothing is printed.
--
-- Introduced in version @0.2.0.0@.
printOptErrors :: Parameters c -> c -> [String] -> IO ()
printOptErrors params cfg errs
  | null errs = return ()
  | otherwise = hPutStrLn handle . unlines
    $ "Error(s) occurred while processing command line options:"
    : map ("  " ++) errs
  where handle = paramOutputHandle params cfg
