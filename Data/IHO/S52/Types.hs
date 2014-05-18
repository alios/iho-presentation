{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types
    ( module Data.IHO.S52.Types.Module
    , module Data.IHO.S52.Types.LBID
    , module Data.IHO.S52.Types.ColourTable      
    , module Data.IHO.S52.Types.LookupTable      
    , Library (..)
    , parseLibrary
    ) where
    
import Data.Attoparsec.Text

import Data.IHO.S52.Types.Module
import Data.IHO.S52.Types.LBID
import Data.IHO.S52.Types.ColourTable
import Data.IHO.S52.Types.LookupTable

    
data Library = 
    Library { lib_id :: Record LibraryId 
            , lib_cols :: [Record ColourTable]
            , lib_lupt :: [Record LookupTable]
            }
            deriving (Show, Eq)

parseLibrary :: Parser Library
parseLibrary = do
  lbid <- module_parser
  cols <- many1 module_parser
  lupt <- many1 module_parser
  return $ Library lbid cols lupt

