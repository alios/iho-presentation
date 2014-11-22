{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.Symbol
    ( Symbol (..)
    , Record (..)
    ) where

import Prelude hiding (take, takeWhile)
import Data.Text (Text)
import Data.Int
import Data.Attoparsec.Text
import Data.IHO.S52.Types.Module
import Data.IHO.S52.Types.Vector
import Data.IHO.S52.Types.Helper
import Data.Map (Map)
import qualified Data.Map as Map

data Symbol

instance Module Symbol where
    data Record Symbol =
        SymbolEntry { symb_modn :: ! Text -- ^ Module Identifier (Module Name)
                    , symb_rcid :: ! Int16 -- ^ Record Identifier
                    , symb_stat :: ! Text -- ^ status of module contents
                    , symb_synm :: ! Text -- ^ Name of the symbol
                    , symb_sydf :: ! DrawingType -- ^ type of the symbold definition
                    , symb_sycl :: ! Int16 -- ^ pivot-point's column-number
                    , symb_syrw :: ! Int16 -- ^ pivot-point's row-number
                    , symb_syhl :: ! Int16 -- ^ width of boundingbox
                    , symb_syvl :: ! Int16 -- ^ height of boundingbox
                    , symb_sbxc :: ! Int16 -- ^ bounding box upper left column number
                    , symb_sbxr :: ! Int16 -- ^ bounding box upper left row number
                    , symb_sxpo :: ! Text -- ^ free text for symbology explanation
                    , symb_scrf :: ! (Map Char Text) -- ^ 'ColourTable' reference map
                    , symb_sbtm :: ! [Text] -- ^ raster image rows
                    , symb_svct :: ! [[VectorInstruction]]
                    } deriving (Eq, Show)
    module_modn = symb_modn
    module_rcid = symb_rcid
    module_stat = symb_stat
    module_parser = do
      rcid' <- parseLine "0001" (take 5)
      (modn, rcid, stat) <-
          parseLine "SYMB" $
                    do modn <- string "SY"
                       rcid <- parseInt16
                       stat <- take 3
                       return $ (modn, rcid, stat)
      (synm, sydf, sycl, syrw, syhl, syvl, sbxc, sbxr) <- 
          parseLine "SYMD" $
                    do synm <- take 8
                       sydf <- parseDrawingType
                       sycl <- parseInt16
                       syrw <- parseInt16
                       syhl <- parseInt16
                       syvl <- parseInt16
                       sbxc <- parseInt16
                       sbxr <- parseInt16
                       return (synm, sydf, sycl, syrw, syhl, syvl, sbxc, sbxr)
      sxpo <- parseLine "SXPO" varString
      scrf <- parseLine "SCRF" $ many' $ do
                            k <- anyChar
                            v <- take 5
                            return (k,v)
             
      sbtm <- case sydf of
               RasterDrawing -> many' $ parseLine "SBTM" $ varString
               VectorDrawing -> return []
      svct <- case sydf of
               RasterDrawing -> return []
               VectorDrawing ->  many' $ parseLine "SVCT" $ parseInstructions
      _ <- parseLine "****" endOfInput  
      return $ SymbolEntry
                 { symb_modn = modn 
                 , symb_rcid = rcid
                 , symb_stat = stat
                 , symb_synm = synm
                 , symb_sydf = sydf
                 , symb_sycl = sycl                               
                 , symb_syrw = syrw
                 , symb_syhl = syhl
                 , symb_syvl = syvl
                 , symb_sbxc = sbxc
                 , symb_sbxr = sbxr
                 , symb_sxpo = sxpo
                 , symb_scrf = Map.fromList  scrf
                 , symb_sbtm = sbtm
                 , symb_svct = svct
                 }

instance VectorRecord Symbol where
    vector_pos s = (symb_sycl s, symb_syrw s)
    vector_box_size s =  (symb_syhl s, symb_syvl s)
    vector_box_pos s = (symb_sbxc s, symb_sbxr s)
    vector_color_refs = symb_scrf
    vector_xpo = symb_sxpo
    vector_vct = symb_svct
    vector_name = symb_synm
