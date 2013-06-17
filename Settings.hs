--Final Project ALP.
--Student: Marcos Pividori

-- |This module provides some useful functions to structure the code in Yesod.

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings(widgetFile,cassiusFile) where

import Prelude
import Language.Haskell.TH.Syntax
import Yesod.Default.Util
import Data.Default (def)
import Text.Hamlet
import qualified Text.Cassius as H

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

widgetFile :: String -> Q Exp
widgetFile = widgetFileReload widgetFileSettings

toCassiusFile x = x ++ ".cassius"

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile = H.cassiusFile . toCassiusFile
#else
cassiusFile = H.cassiusFileDebug . toCassiusFile
#endif

