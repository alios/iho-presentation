{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.IHO.S52.QuasiQuote where

import Data.IHO.S52.Types
import qualified Data.Text as T
import Language.Haskell.TH.Quote


parseLibrary' :: String -> Library
parseLibrary' = either error id . parseLibrary . T.pack


presLib :: QuasiQuoter
presLib = QuasiQuoter
    { quoteExp = \s -> [| parseLibrary' s |]
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined       
    }

presLibFile :: QuasiQuoter
presLibFile = quoteFile presLib
