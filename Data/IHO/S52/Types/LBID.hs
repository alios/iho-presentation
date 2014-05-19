{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.LBID
    ( LibraryId (..)
    , Record (..)
    , lbid_compileTime 
    ) where

import Prelude hiding (take, takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int
import Data.Attoparsec.Text
import Data.Time.Calendar
import Data.Time.Clock


import Data.IHO.S52.Types.Module
import Data.IHO.S52.Types.Helper

data LibraryId

instance Module LibraryId where
    data Record LibraryId =
        LibraryId { lbid_modn :: ! Text --  Module Name
                  , lbid_rcid :: ! Int16 --  Record Identifier 
                  , lbid_expp :: ! Text --  Exchange Purpose 
                  , lbid_ptyp :: ! Text --  Product Type 
                  , lbid_esid :: ! Int --  Exchange Set Identification Number 
                  , lbid_edtn :: ! Float --  Edition Number
                  , lbid_codt :: ! Day --  Compilation Date of Exchange Set
                  , lbid_coti :: ! DiffTime --  Compilation Time of Exchange Set
                  , lbid_vrdt :: ! Day --  Library-Profile Versions Date
                  , lbid_prof :: ! Text --  Library Application Profile
                  , lbid_ocdt :: ! Day --  Date of Version of applied Object Catalogue
                  , lbid_comt :: ! Text --  Comment
                  } deriving (Show, Eq)
    module_modn = lbid_modn
    module_rcid = lbid_rcid
    module_stat = error "stat is undefined for LBID records"
    module_parser = do
      rcid' <- fmap (read . T.unpack) $ parseLine "0001" (take 5)
      (modn, rcid, expp, ptyp, esid, edtn, codt, coti,vrdt, prof, ocdt, comt) <-
          parseLine "LBID" $
                    do modn <- string "LI"
                       rcid <- parseInt16
                       expp <- take 3
                       ptyp <- varString
                       esid <- fmap (read . T.unpack) varString
                       edtn <- fmap (read . T.unpack) varString
                       codt <- parseDay
                       coti <- parseTime
                       vrdt <- parseDay
                       prof <- take 2
                       ocdt <- parseDay
                       comt <- varString
                       return (modn, rcid, expp, ptyp, esid, edtn, codt, coti,vrdt, prof, ocdt, comt)
      let tests = filter (not . fst)
              [ (rcid == rcid', "record ids mismatch: " ++ show rcid' ++ " / " ++ show rcid)
              , (vrdt <= codt, "vrdt " ++ show vrdt ++ " must be before codt " ++ show codt)
              , (vrdt <= ocdt, "vrdt " ++ show vrdt ++ " must be before ocdt " ++ show ocdt)
              , (ocdt <= codt, "ocdt " ++ show ocdt ++ " must be before codt " ++ show codt)
              ]
      case tests of
        [] -> do { _ <- parseLine "****" endOfInput  
                ; return $ LibraryId modn rcid expp ptyp esid edtn codt coti vrdt prof ocdt comt 
                }
        (err:_) -> fail $ snd err

lbid_compileTime :: Record LibraryId -> UTCTime
lbid_compileTime lib = UTCTime (lbid_codt lib) (lbid_coti lib)
