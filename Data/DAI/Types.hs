{-# LANGUAGE OverloadedStrings #-}

module Data.DAI.Types
    ( Library (..)
    , parseLibrary
    , module Data.DAI.Types.Module
    , module Data.DAI.Types.LBID
    , module Data.DAI.Types.ColourTable      
    ) where
    
import Data.Attoparsec.Text

import Data.DAI.Types.Module
import Data.DAI.Types.LBID
import Data.DAI.Types.ColourTable
    
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

