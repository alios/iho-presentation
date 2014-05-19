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
import Data.IHO.S52.Types.Helper
import qualified Data.Map as Map
import qualified Data.Set as Set

data Symbol

instance Module Symbol where
    data Record Symbol =
        SymbolEntry { symb_modn :: ! Text 
                    , symb_rcid :: ! Int16
                    , symb_stat :: ! Text
                    , symb_synm :: ! Text
                    , symb_sydf :: ! DrawingType
                    , symb_sycl :: ! Int16                             
                    , symb_syrw :: ! Int16
                    , symb_syhl :: ! Int16
                    , symb_syvl :: ! Int16
                    , symb_sbxc :: ! Int16
                    , symb_sbxr :: ! Int16
                    , symb_sxpo :: ! Text
                    , symb_scrf :: ! [(Char, Text)]
                    , symb_sbtm :: ! [Text]
                    , symb_svct :: ! [[VectorInstruction]]
                    } deriving (Eq, Show)
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
               VectorDrawing ->  many' $ parseLine "SVCT" $ many' $ do
                                  c <- takeWhile $ notInClass ";"
                                  skip $ inClass ";"
                                  return c

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
                 , symb_scrf = scrf
                 , symb_sbtm = sbtm
                 , symb_svct = svct
                 }

instance VectorRecord Symbol where
    vector_pos s = (symb_sycl s, symb_syrw s)
    vector_box_size s =  (symb_syhl s, symb_syvl s)
    vector_box_pos s = (symb_sbxc s, symb_sbxr s)
    vector_color_refs = Map.fromList . symb_scrf
    vector_xpo = symb_sxpo
    vector_vct = Set.fromList . symb_svct
