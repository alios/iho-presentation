{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.LineStyle
    ( LineStyle (..)
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


data LineStyle

instance Module LineStyle where
    data Record LineStyle =
        LineStyleEntry { lnst_modn :: ! Text 
                       , lnst_rcid :: ! Int16
                       , lnst_stat :: ! Text
                       , lnst_linm :: ! Text
                       , lnst_licl :: ! Int16                               
                       , lnst_lirw :: ! Int16
                       , lnst_lihl :: ! Int16
                       , lnst_livl :: ! Int16
                       , lnst_lbxc :: ! Int16
                       , lnst_lbxr :: ! Int16    
                       , lnst_lxpo :: ! Text
                       , lnst_lcrf :: ! [(Char, Text)]
                       , lnst_lvct :: ! [[Text]]
                       } deriving (Show, Eq)
    module_modn = lnst_modn
    module_rcid = lnst_rcid
    module_stat = lnst_stat
    module_parser = do
      rcid' <- parseLine "0001" (take 5)
      (modn, rcid, stat) <-
          parseLine "LNST" $
                    do modn <- string "LS"
                       rcid <- parseInt16
                       stat <- take 3
                       return $ (modn, rcid, stat)
      (linm, licl, lirw, lihl, livl, lbxc, lbxr) <-
          parseLine "LIND" $
                    do linm <- take 8
                       licl <- parseInt16
                       lirw <- parseInt16
                       lihl <- parseInt16
                       livl <- parseInt16
                       lbxc <- parseInt16
                       lbxr <- parseInt16
                       return (linm, licl, lirw, lihl, livl, lbxc, lbxr)
      lxpo <- parseLine "LXPO" varString
      lcrf <- parseLine "LCRF" $ many' $ do
                            k <- anyChar
                            v <- take 5
                            return (k,v)
      lvct <- many' $ parseVectorInstructions "LVCT"      
      _ <- parseLine "****" endOfInput  
      return $ LineStyleEntry
                 { lnst_modn = modn 
                 , lnst_rcid = rcid
                 , lnst_stat = stat
                 , lnst_linm = linm
                 , lnst_licl = licl                               
                 , lnst_lirw = lirw
                 , lnst_lihl = lihl
                 , lnst_livl = livl
                 , lnst_lbxc = lbxc
                 , lnst_lbxr = lbxr
                 , lnst_lxpo = lxpo
                 , lnst_lcrf = lcrf
                 , lnst_lvct = lvct
                 }

instance VectorRecord LineStyle where
    vector_pos s = (lnst_licl s, lnst_lirw s)
    vector_box_size s = (lnst_lihl s, lnst_livl s)
    vector_box_pos s = (lnst_lbxc s, lnst_lbxr s)
    vector_color_refs = Map.fromList . lnst_lcrf 
    vector_xpo = lnst_lxpo
    vector_vct = Set.fromList . lnst_lvct
    vector_name = lnst_linm
                                  
