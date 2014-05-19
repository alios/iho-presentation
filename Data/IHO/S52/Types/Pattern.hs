{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.Pattern
    ( Pattern (..)
    , Record (..)
    , patt_min_space
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
import qualified Data.Map as Map
import qualified Data.Set as Set


data Pattern

data FillPattern = 
    StaggeredPattern | LinearPattern
    deriving (Eq, Show)

parseFillPattern :: Parser FillPattern
parseFillPattern = do
  df <- choice
       [ try $ string "STG"
       , try $ string "LIN"
       ]
  return $ case df of
             "STG" -> StaggeredPattern
             "LIN" -> LinearPattern
             _ -> error "unknown FillPattern"

data PatternSpacing = 
    ConstantSpace | ScaleDependentPattern
    deriving (Eq, Show)

parsePatternSpacing :: Parser PatternSpacing
parsePatternSpacing = do
  df <- take 3
  return $ case df of
             "STG" -> ConstantSpace
             "LIN" -> ScaleDependentPattern
             _ -> error "unknown PatternSpacing"


instance Module Pattern where
    data Record Pattern =
        PatternEntry { patt_modn :: ! Text 
                     , patt_rcid :: ! Int16
                     , patt_stat :: ! Text
                     , patt_panm :: ! Text
                     , patt_padf :: ! DrawingType
                     , patt_patp :: ! FillPattern
                     , patt_pasp :: ! PatternSpacing
                     , patt_pami :: ! Int16
                     , patt_pama :: ! Int16
                     , patt_pacl :: ! Int16                             
                     , patt_parw :: ! Int16
                     , patt_pahl :: ! Int16
                     , patt_pavl :: ! Int16
                     , patt_pbxc :: ! Int16
                     , patt_pbxr :: ! Int16
                     , patt_pxpo :: ! Text
                     , patt_pcrf :: ! [(Char, Text)]
                     , patt_pbtm :: ! [Text]
                     , patt_pvct :: ! [[Text]]
                     } deriving (Show, Eq)
    module_modn = patt_modn
    module_rcid = patt_rcid
    module_stat = patt_stat
    module_parser = do
      rcid' <- parseLine "0001" (take 5)
      (modn, rcid, stat) <-
          parseLine "PATT" $
                    do modn <- string "PT"
                       rcid <- parseInt16
                       stat <- take 3
                       return $ (modn, rcid, stat)
      (panm, padf, patp, pasp, pami, pama, pacl, parw, pahl, pavl, pbxc, pbxr) <- 
          parseLine "PATD" $
                    do panm <- take 8
                       padf <- parseDrawingType
                       patp <- parseFillPattern
                       pasp <- parsePatternSpacing
                       pami <- parseInt16
                       pama <- parseInt16
                       pacl <- parseInt16
                       parw <- parseInt16
                       pahl <- parseInt16
                       pavl <- parseInt16
                       pbxc <- parseInt16
                       pbxr <- parseInt16
                       return (panm, padf, patp, pasp, pami, pama, pacl, parw, pahl, pavl, pbxc, pbxr)
      pxpo <- parseLine "PXPO" varString
      pcrf <- parseLine "PCRF" $ many' $ do
                            k <- anyChar
                            v <- take 5
                            return (k,v)
      pbtm <- case padf of
               RasterDrawing -> many' $ parseLine "PBTM" $ varString
               VectorDrawing -> return []
      pvct <- case padf of
               RasterDrawing -> return []
               VectorDrawing -> many' $ parseVectorInstructions "PVCT"
      _ <- parseLine "****" endOfInput  
      return $ PatternEntry
                 { patt_modn = modn 
                 , patt_rcid = rcid
                 , patt_stat = stat
                 , patt_panm = panm
                 , patt_padf = padf
                 , patt_patp = patp
                 , patt_pasp = pasp
                 , patt_pami = pami
                 , patt_pama = pama
                 , patt_pacl = pacl                               
                 , patt_parw = parw
                 , patt_pahl = pahl
                 , patt_pavl = pavl
                 , patt_pbxc = pbxc
                 , patt_pbxr = pbxr
                 , patt_pxpo = pxpo
                 , patt_pcrf = pcrf
                 , patt_pbtm = pbtm
                 , patt_pvct = pvct
                 }

patt_min_space :: Record Pattern -> (Int16, Int16)
patt_min_space s = ( patt_pami s, patt_pama s)
 
instance VectorRecord Pattern where
    vector_pos s = (patt_pacl s, patt_parw s)
    vector_box_size s = (patt_pahl s, patt_pavl s)
    vector_box_pos s = (patt_pbxc s, patt_pbxr s)
    vector_color_refs = Map.fromList . patt_pcrf 
    vector_xpo = patt_pxpo
    vector_vct = Set.fromList . patt_pvct
    vector_name = patt_panm
