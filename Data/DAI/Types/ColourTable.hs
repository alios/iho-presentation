{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.DAI.Types.ColourTable
    ( ColourTable (..)
    , ColourMapEntry
    , ColourMap
    , Record (..)
    , cols_lookup 
    ) where

import Prelude hiding (take, takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int
import Data.Attoparsec.Text
import Data.Time.Calendar
import Data.Time.Clock
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Prizm.Types
import Data.Prizm.Color.CIE.XYZ


import Data.DAI.Types.Module
import Data.DAI.Types.Helper

data ColourTable

type ColourMapEntry = (CIEXYZ Double, Text)
type ColourMap = Map Text ColourMapEntry

instance Module ColourTable where
    data Record ColourTable =
       ColourTable { cols_modn :: ! Text
                   , cols_rcid :: ! Int16
                   , cols_stat :: ! Text
                   , cols_ctus :: ! Text
                   , cols_entries :: ! ColourMap
                   } deriving (Show, Eq)
    module_parser = do 
      rcid' <- fmap (read . T.unpack) $ parseLine "0001" (take 5)
      (modn, rcid, stat, ctus) <-
          parseLine "COLS" $
                    do modn <- string "CS"
                       rcid <- parseInt16
                       stat <- take 3
                       ctus <- varString
                       return (modn, rcid, stat, ctus)
      rs <- many' $ try $ parseLine "CCIE" $ do
                              ctok <- take 5
                              chrx <- parseDouble
                              chry <- parseDouble
                              clum <- parseDouble
                              cuse <- varString
                              return $ (ctok, (xyYPrism (chrx, chry, clum), cuse))
      let tests = filter (not . fst) [ (rcid == rcid', "record ids mismatch: " ++ show rcid' ++ " / " ++ show rcid) ]
      case tests of
        [] -> do { _ <- parseLine "****" endOfInput  
                ; return $ ColourTable modn rcid stat ctus (Map.fromList rs)
                }
        (err:_) -> fail $ snd err                      
                  
cols_lookup :: Text -> Text -> [Record ColourTable] -> Maybe ColourMapEntry
cols_lookup ctus ctok ts = do
  t <- find (\t -> cols_ctus t == ctus) ts
  Map.lookup ctok $ cols_entries t
          

