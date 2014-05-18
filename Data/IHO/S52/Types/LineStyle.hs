{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.LineStyle
    ( LineStyle (..)
    , Record (..)
    ) where

import Prelude hiding (take, takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Data.Attoparsec.Text
import Data.IHO.S52.Types.Module
import Data.IHO.S52.Types.Helper


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
                       , lnst_lbtm :: ! [Text]
                       , lnst_lvct :: ! [[Text]]
                       } deriving (Show, Eq)
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
      lbtm <- many' $ parseLine "LBTM" $ varString
      lvct <- many' $ parseLine "LVCT" $ many' $ do
                            c <- takeWhile $ notInClass ";"
                            skip $ inClass ";"
                            return c
      
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
                 , lnst_lbtm = lbtm
                 , lnst_lvct = lvct
                 }
