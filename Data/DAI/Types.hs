{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.DAI.Types ( Library(..), parseLibrary
                      , Module(..)
                      , LibraryId (..), lbid_compileTime
                      , ColourTable(..), ColourMap, ColourMapEntry, cols_lookup
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

class Module m where
    data Record m :: *
    module_parser :: Parser (Record m)
    
data Library = 
    Library { lib_id :: Record LibraryId 
            , lib_cols :: [Record ColourTable]
            }
            deriving (Show, Eq)

parseLibrary :: Parser Library
parseLibrary = do
  id <- module_parser
  cols <- many' module_parser
  return $ Library id cols

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
                              return $ (ctok, (constPrism (chrx, chry, clum), cuse))
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
          

data LibraryId

instance Module LibraryId where
    data Record LibraryId =
        LibraryId { lbid_modn :: ! Text -- | Module Name
                  , lbid_rcid :: ! Int16 -- | Record Identifier 
                  , lbid_expp :: ! Text -- | Exchange Purpose 
                  , lbid_ptyp :: ! Text -- | Product Type 
                  , lbid_esid :: ! Int -- | Exchange Set Identification Number 
                  , lbid_edtn :: ! Float -- | Edition Number
                  , lbid_codt :: ! Day -- | Compilation Date of Exchange Set
                  , lbid_coti :: ! DiffTime -- | Compilation Time of Exchange Set
                  , lbid_vrdt :: ! Day -- | Library-Profile Versions Date
                  , lbid_prof :: ! Text -- | Library Application Profile
                  , lbid_ocdt :: ! Day -- | Date of Version of applied Object Catalogue
                  , lbid_comt :: ! Text -- | Comment

                  } deriving (Show, Eq)
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



constPrism (x,y,z) = CIEXYZ x y z
xyYPrism = constPrism . xyY2XYZ

xyY2XYZ :: Fractional t => (t, t, t) -> (t, t, t)
xyY2XYZ (x,y,l) = 
    let x' = x * (l / y)
        y' = y * (l / y)
        z' = (1 - x - y) * (l / y)
    in (x', y', z')

i2i :: (Integral a, Num b) => a -> b
i2i = fromInteger . toInteger

varString :: Parser Text
varString = do
  xs <- takeWhile $ notInClass "\US"
  e <- atEnd
  if (e) then return xs
  else do skip $ inClass "\US"
          return xs

parseDouble :: Parser Double 
parseDouble = fmap (read . T.unpack) varString

parseInt16 :: Parser Int16
parseInt16 = fmap (read . T.unpack) $ take 5 

parseDay :: Parser Day
parseDay = do
  y <- fmap (read . T.unpack) $ take 4
  m <- fmap (read . T.unpack) $ take 2
  d <- fmap (read . T.unpack) $ take 2
  case (fromGregorianValid y m d) of
    Nothing -> fail $ "invalid date: " ++ concat [show y,"-",show m,"-", show d]
    Just d -> return d

parseTime :: Parser DiffTime
parseTime = do
  h <- fmap (read . T.unpack) $ take 2
  m <- fmap (read . T.unpack) $ take 2
  s <- fmap (read . T.unpack) $ take 2
  return $ secondsToDiffTime $ (h * 60 + m) * 60 + s

parseLine :: Text -> Parser t -> Parser t
parseLine fn vp
    | (T.length fn /= 4) =
        fail $ "fieldname must have length of 4. given: " ++
             T.unpack fn
    | otherwise = 
        do fn' <- take 4 <?> "field name"
           if (fn' /= fn) then fail $ "expected " ++ T.unpack fn ++ " read " ++ T.unpack fn'
           else do
             vl <- parseInt16 <?> "field value length"
             rawf <- (take $ i2i vl) <?> "raw field"
             _ <- string "\r\n"
             case (parseOnly vp rawf) of
               Left err -> fail err
               Right t -> return t
           

  
