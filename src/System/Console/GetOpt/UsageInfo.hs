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
    ( renderUsageInfo
    , usageInfo
    , formatOptions
    , formatOption
    , formatShortOption
    , formatLongOption
    )
    where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), OptDescr(..))

import qualified System.Console.Terminal.Size as Terminal (Window(width), size)

import Text.PrettyPrint.HughesPJ


renderUsageInfo :: String -> [OptDescr a] -> IO String
renderUsageInfo header opts = do
    terminalWidth <- fmap Terminal.width <$> Terminal.size
    return . renderStyle style{lineLength = fromMaybe 80 terminalWidth}
        $ usageInfo header opts

usageInfo :: String -> [OptDescr a] -> Doc
usageInfo header opts = (fsep . map text $ words header) $+$ text ""
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

formatShortOption :: ArgDescr a -> Char -> Doc
formatShortOption argDescr opt = char '-' <> char opt <+> case argDescr of
    NoArg _ -> empty
    OptArg _ s -> brackets $ text s
    ReqArg _ s -> text s

formatLongOption :: ArgDescr a -> String -> Doc
formatLongOption argDescr opt = text "--" <> text opt <> case argDescr of
    NoArg _ -> empty
    OptArg _ s -> brackets $ equals <> text s
    ReqArg _ s -> equals <> text s
