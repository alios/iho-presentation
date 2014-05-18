{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.LookupTable
    ( LookupTable (..)
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


data LookupTable

type LookupMap = Map Text Text


parseI :: Parser Text
parseI = parseI' []

parseI' :: String -> Parser Text
parseI' is = do
  eof <- atEnd
  if (eof) then return $ T.pack is
  else do
    i <- anyChar
    if (i == ';') then return $ T.pack is
    else parseI' (is ++ [i])
    
parseInstr :: [Text] -> Parser [Text]
parseInstr is = do
  i <- parseI
  eof <- atEnd
  if(eof) then return (is ++ [i])
  else parseInstr (is ++ [i])


instance Module LookupTable where
    data Record LookupTable = 
        LookupTableEntry { lupt_modn :: ! Text
                         , lupt_rcid :: ! Int16
                         , lupt_stat :: ! Text
                         , lupt_obcl :: ! Text
                         , lupt_ftyp :: ! Char
                         , lupt_dpri :: ! Int16
                         , lupt_rpri :: ! Char
                         , lupt_tnam :: ! Text
                         , lupt_attc :: ! [(Text, Text)]
                         , lupt_inst :: ! [Text]
                         , lupt_disc :: ! Text
                         , lupt_lucm :: ! Text
                         } deriving (Show, Eq)

    module_parser = do
      rcid' <- parseLine "0001" (take 5)
      (modn, rcid, stat, obcl, ftyp, dpri, rpri, tnam) <-
          parseLine "LUPT" $
                    do modn <- string "LU"
                       rcid <- parseInt16
                       stat <- take 3
                       obcl <- take 6
                       ftyp <- satisfy $ inClass "ALP"
                       dpri <- parseInt16
                       rpri <- satisfy $ inClass "OS"
                       tnam <- varString
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

  
