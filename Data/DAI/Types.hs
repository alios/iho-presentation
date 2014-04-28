{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.DAI.Types ( ) where
    
import Prelude hiding (take, takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int
import Data.Attoparsec.Text

class Module m where
    data Record m :: *

    module_parser :: Parser (Record m)
    

data LibraryId

instance Module LibraryId where
    data Record LibraryId =
        LibraryId { lbid_modn :: Text -- | Module Name
                  , lbid_rcid :: Int16 -- | Record Identifier 
                  , lbid_expp :: Text -- | Exchange Purpose 
                  , lbid_ptyp :: Text -- | Product Type 
                  , lbid_esid :: Text -- | Exchange Set Identification Number 
                  , lbid_edtn :: Text -- | Edition Number
                  , lbid_codt :: Text -- | Compilation Date of Exchange Set
                  , lbid_coti :: Text -- | Compilation Time of Exchange Set
                  , lbid_vrdt :: Text -- | Library-Profile Versions Date
                  , lbid_prof :: Text -- | Library Application Profile
                  , lbid_ocdt :: Text -- | Date of Version of applied Object Catalogue
                  , lbid_comt :: Text -- | Comment
                  }
    module_parser = do
      rcid' <- parseLine "0001" (take 5)
      (modn, rcid, expp, ptyp, esid, edtn, codt, coti,vrdt, prof, ocdt, comt) <-
          parseLine "LBID" $
                    do modn <- string "LI"
                       rcid <- parseInt16
                       expp <- take 3
                       ptyp <- varString
                       esid <- varString
                       edtn <- varString
                       codt <- take 8
                       coti <- take 6
                       vrdt <- take 8
                       prof <- take 2
                       ocdt <- take 8
                       comt <- varString
                       return (modn, rcid, expp, ptyp, esid, edtn, codt, coti,vrdt, prof, ocdt, comt)
      parseLine "****" endOfInput
      return $ LibraryId modn rcid expp ptyp esid edtn codt coti vrdt prof ocdt comt 

i2i :: (Integral a, Num b) => a -> b
i2i = fromInteger . toInteger

varString :: Parser Text
varString = do
  xs <- takeWhile $ notInClass "\US"
  e <- atEnd
  if (e) then return xs
  else do skip $ inClass "\US"
          return xs
      
parseInt16 :: Parser Int16
parseInt16 = fmap (read . T.unpack) $ take 5 
             
parseLine :: Text -> Parser t -> Parser t
parseLine fn vp
    | (T.length fn /= 4) =
        fail $ "fieldname must have length of 4. given: " ++
             T.unpack fn
    | otherwise = 
        do _ <- string fn <?> "field name"
           vl <- parseInt16 <?> "field value length"
           rawf <- (take $ i2i vl) <?> "raw field"
           case (parseOnly vp rawf) of
             Left err -> fail err
             Right t -> return t
           

  
