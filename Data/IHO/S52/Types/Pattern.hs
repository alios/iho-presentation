{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.Pattern
    ( Pattern (..)
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


data Pattern

instance Module Pattern where
    data Record Pattern =
        PatternEntry { patt_modn :: ! Text 
                     , patt_rcid :: ! Int16
                     , patt_stat :: ! Text
                     , patt_panm :: ! Text
                     , patt_padf :: ! Char
                     , patt_patp :: ! Text
                     , patt_pasp :: ! Text
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
                       padf <- satisfy $ inClass "VR"
                       patp <- take 3
                       pasp <- take 3
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
      pbtm <- many' $ parseLine "PBTM" $ varString
      pvct <- many' $ parseLine "PVCT" $ many' $ do
                            c <- takeWhile $ notInClass ";"
                            skip $ inClass ";"
                            return c


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
