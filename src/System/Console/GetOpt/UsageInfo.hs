-- |
-- Module:       $HEADER$
-- Description:  Alternative to System.Console.GetOpt.usageInfo.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Alternative to System.Console.GetOpt.usageInfo.
module System.Console.GetOpt.UsageInfo
    (
    -- * Render usage info
      UsageInfoConfig(..)
    , renderUsageInfo

    -- * Format usage info in to Doc
    , formatUsageInfo
    , formatOptions
    , formatOption
    , formatShortOption
    , formatLongOption
    )
    where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), OptDescr(..))
import System.IO (Handle, stdout)

import Data.Default.Class (Default(def))
import qualified System.Console.Terminal.Size
    as Terminal (Window(width), hSize)
import Text.PrettyPrint.HughesPJ


-- | Parameters for 'renderUsageInfo'.
data UsageInfoConfig = UsageInfoConfig
    { defaultLineLength :: Int
    -- ^ Line length used when output handle is not a terminal.
    , outputHandle :: Handle
    -- ^ Output handle to use for getting terminal size. Usage info is not
    -- written to it.
    }

instance Default UsageInfoConfig where
    def = UsageInfoConfig
        { defaultLineLength = 80
        , outputHandle = stdout
        }

-- | Render usage information, produced by 'usageInfo', using terminal width as
-- line length. If 'outputHandle' is not a terminal, then 'defaultLineLength'
-- is used.
renderUsageInfo :: UsageInfoConfig -> String -> [OptDescr a] -> IO String
renderUsageInfo cfg header opts = do
    terminalWidth <- fmap Terminal.width <$> Terminal.hSize (outputHandle cfg)
    return . renderStyle style{lineLength = getLineLength terminalWidth}
        $ formatUsageInfo header opts
  where
    getLineLength = fromMaybe (defaultLineLength cfg)

formatUsageInfo :: String -> [OptDescr a] -> Doc
formatUsageInfo header opts = (fsep . map text $ words header) $+$ text ""
    $+$ formatOptions opts

formatOptions :: [OptDescr a] -> Doc
formatOptions = (text "Options:" $+$)
    . foldl (\ x y -> x $+$ text "" $+$ y) empty . map formatOption

formatOption :: OptDescr a -> Doc
formatOption (Option short long argDescr descStr) =
    nest 2 $ synDoc $+$ nest 4 descDoc
  where
    synDoc = hsep . punctuate comma
        $ map (formatShortOption argDescr) short
        ++ map (formatLongOption argDescr) long
    descDoc = fsep . map text $ words descStr

-- | Format short option:
--
-- > '-' <option> (<space> (<required-argument>|'[' <optional-argument> ']'))?
formatShortOption :: ArgDescr a -> Char -> Doc
formatShortOption argDescr opt = char '-' <> char opt <+> case argDescr of
    NoArg _ -> empty
    OptArg _ s -> brackets $ text s
    ReqArg _ s -> text s

-- | Format long option:
--
-- > "--" <option> ('=' (<required-argument>|'[' <optional-argument> ']'))?
formatLongOption :: ArgDescr a -> String -> Doc
formatLongOption argDescr opt = text "--" <> text opt <> case argDescr of
    NoArg _ -> empty
    OptArg _ s -> brackets $ equals <> text s
    ReqArg _ s -> equals <> text s
