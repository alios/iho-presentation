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

instance Module LookupTable where
    data Record LookupTable = 
        LookupTableEntry { lupt_modn :: ! Text
                         , lupt_rcid :: ! Int16
                         , lupt_stat :: ! Text
                         , lupt_obcl :: ! Text
                         , lupt_ftyp :: ! Char
                         , lupt_attc :: ! [(Text, Text)]
                         , lupt_inst :: ! Text 
                         , lupt_disc :: ! Text
                         , lupt_lucm :: ! Text
                         } deriving (Show, Eq)

    module_parser = do
      rcid' <- fmap (read . T.unpack) $ parseLine "0001" (take 5)
      (modn, rcid, stat, obcl, ftyp, dpri, tnam) <-
          parseLine "LUPT" $
                    do modn <- string "LU"
                       rcid <- parseInt16
                       stat <- take 3
                       obcl <- take 6
                       ftyp <- satisfy $ inClass "AL"
                       dpri <- parseInt16
                       tnam <- varString
                       return (modn, rcid, stat, obcl, ftyp, dpri, tnam)
      attc <- parseLine "ATTC" $ many' $ do
                              attl <- take 6
                              attv <- varString
                              return (attl, attv)
      inst <- parseLine "INST" $ varString
      disc <- parseLine "DISC" $ varString
      lucm <- parseLine "LUCM" $ varString

      let tests = filter (not . fst) [ (rcid == rcid', "record ids mismatch: " ++ show rcid' ++ " / " ++ show rcid) ]
      case tests of
        [] -> do { _ <- parseLine "****" endOfInput  
                ; return $ LookupTableEntry
                             { lupt_modn = modn 
                             , lupt_rcid = rcid
                             , lupt_stat = stat 
                             , lupt_obcl = obcl
                             , lupt_ftyp = ftyp
                             , lupt_attc = attc
                             , lupt_inst = inst
                             , lupt_disc = disc
                             , lupt_lucm = lucm
                             }
                }
        (err:_) -> fail $ snd err                      

  
