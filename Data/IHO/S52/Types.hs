{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types
    ( Library (..)
    , parseLibrary
    , module Data.IHO.S52.Types.Module
    , module Data.IHO.S52.Types.LBID
    , module Data.IHO.S52.Types.ColourTable      
    ) where
    
import Data.Attoparsec.Text

import Data.IHO.S52.Types.Module
import Data.IHO.S52.Types.LBID
import Data.IHO.S52.Types.ColourTable
    
data Library = 
    Library { lib_id :: Record LibraryId 
            , lib_cols :: [Record ColourTable]
            }
            deriving (Show, Eq)

parseLibrary :: Parser Library
parseLibrary = do
  lbid <- module_parser
  cols <- many' module_parser
  return $ Library lbid cols

