{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.LookupTable
    ( LookupTable (..)
    , Record (..)
    , FTYP (..)
    , RPRI (..)
    , TNAM (..)
    ) where

import Prelude hiding (take, takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int
import Data.Attoparsec.Text
import Data.IHO.S52.Types.Module
import Data.IHO.S52.Types.Helper


data LookupTable


data FTYP = Area | Point | Line deriving (Eq, Show)

parseFTYP :: Parser FTYP
parseFTYP = do
  ftyp <- satisfy $ inClass "ALP"
  return $ case ftyp of
             'A' -> Area
             'L' -> Line
             'P' -> Point
             _ -> error "unknown FTYP"

data TNAM = PLAIN_BOUNDARIES | SYMBOLIZED_BOUNDARIES
          | LINES
          | SIMPLIFIED | PAPER_CHART
            deriving (Show, Eq)

data RPRI = OnTopOfRadar | SupressedByRadar deriving (Eq, Show)

tnamP t = try $ do _ <- string $ T.pack . show $ t ; return t
          

parseTNAM :: FTYP -> Parser TNAM
parseTNAM Area = 
    choice  [ tnamP PLAIN_BOUNDARIES                
            , tnamP SYMBOLIZED_BOUNDARIES
            ]
parseTNAM Line = choice [ tnamP LINES]
parseTNAM Point = 
    choice  [ tnamP SIMPLIFIED
            , tnamP PAPER_CHART
            ]

parseRPRI :: Parser RPRI
parseRPRI = do
  rpri <- satisfy $ inClass "OS"
  return $ case rpri of
             'O' -> OnTopOfRadar
             'S' -> SupressedByRadar
             _ -> error "unknown RPRI"


instance Module LookupTable where
    data Record LookupTable = 
        LookupTableEntry { lupt_modn :: ! Text
                         , lupt_rcid :: ! Int16
                         , lupt_stat :: ! Text
                         , lupt_obcl :: ! Text
                         , lupt_ftyp :: ! FTYP
                         , lupt_dpri :: ! Int16
                         , lupt_rpri :: ! RPRI
                         , lupt_tnam :: ! TNAM
                         , lupt_attc :: ! [(Text, Text)]
                         , lupt_inst :: ! [Text]
                         , lupt_disc :: ! Text
                         , lupt_lucm :: ! Text
                         } deriving (Show, Eq)
    module_modn = lupt_modn
    module_rcid = lupt_rcid
    module_stat = lupt_stat
    module_parser = do
      rcid' <- parseLine "0001" (take 5)
      (modn, rcid, stat, obcl, ftyp, dpri, rpri, tnam) <-
          parseLine "LUPT" $
                    do modn <- string "LU"
                       rcid <- parseInt16
                       stat <- take 3
                       obcl <- take 6
                       ftyp <- parseFTYP
                       dpri <- parseInt16
                       rpri <- parseRPRI
                       tnam <- parseTNAM ftyp
                       return (modn, rcid, stat, obcl, ftyp, dpri, rpri, tnam)
      attc <- parseLine "ATTC" $ many' $ do
                              attl <- take 6
                              attv <- varString
                              return (attl, attv)
      inst <- parseLine "INST" $ parseInstr []
      disc <- parseLine "DISC" $ varString
      lucm <- parseLine "LUCM" $ varString
      _ <- parseLine "****" endOfInput  
      return $ LookupTableEntry
                 { lupt_modn = modn 
                 , lupt_rcid = rcid
                 , lupt_stat = stat 
                 , lupt_obcl = obcl
                 , lupt_ftyp = ftyp
                 , lupt_dpri = dpri
                 , lupt_rpri = rpri
                 , lupt_tnam = tnam
                 , lupt_attc = attc
                 , lupt_inst = inst
                 , lupt_disc = disc
                 , lupt_lucm = lucm
                 }

  
