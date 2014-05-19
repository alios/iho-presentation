{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.Helper where

import Prelude hiding (take, takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int
import Data.Attoparsec.Text
import Data.Time.Calendar
import Data.Time.Clock
import Data.Prizm.Types


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


constPrism :: (a, a, a) -> CIEXYZ a
constPrism (x,y,z) = CIEXYZ x y z

xyYPrism :: (Double, Double, Double) -> CIEXYZ Double
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
           

skipSpaces :: Parser ()
skipSpaces = do
  _ <- many' $ try $ skip $ \c -> isHorizontalSpace c || inClass "\r\n" c 
  return ()
