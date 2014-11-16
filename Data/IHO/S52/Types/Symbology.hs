{-# Language DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.IHO.S52.Types.Symbology where
import Control.Lens
import Prelude hiding (take)
import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Char (isDigit)
data HJUST = HCENTRE | RIGHT | LEFT deriving (Data, Typeable, Show, Eq)
makeClassy ''HJUST

data VJUST = BOTTOM | VCENTRE | TOP deriving (Data, Typeable, Show, Eq)
makeClassy ''VJUST
                                             
data SPACE = Fit | StandardSpace | StandardSpaceWrap deriving (Data, Typeable, Show, Eq)
makeClassy ''SPACE

data PSTYLE = SOLD | DASH | DOTT deriving (Data, Typeable, Show, Eq)
makeClassy ''PSTYLE

data FontFamily = PlainSerif  deriving (Data, Typeable, Show, Eq)
makeClassy ''FontFamily

data FontWeight = Light | Medium | Bold   deriving (Data, Typeable, Show, Eq)
makeClassy ''FontWeight 

data FontStyle = NonItallic deriving (Data, Typeable, Show, Eq)
makeClassy ''FontStyle

data CHARS = CHARS { _charsFontFamily :: FontFamily
                   , _charsFontWeight :: FontWeight
                   , _charsFontStyle :: FontStyle
                   , _charsBodySize :: Int
                   } deriving (Data, Typeable, Show, Eq)
makeClassy ''CHARS
instance HasFontFamily CHARS where fontFamily = charsFontFamily
instance HasFontWeight CHARS where fontWeight = charsFontWeight
instance HasFontStyle CHARS  where fontStyle  = charsFontStyle

data SymbologyCommand 
  = TX { _tx_string :: Text
       , _tx_hjust :: HJUST
       , _tx_vjust :: VJUST
       , _tx_space :: SPACE
       , _tx_chars :: CHARS
       , _tx_xoffs :: Int
       , _tx_yoffs :: Int
       , _tx_colour :: Text
       , _tx_display :: Int       
       }
  | TE { _te_format :: Text
       , _te_attribs :: [Text]
       , _te_hjust :: HJUST
       , _te_vjust :: VJUST
       , _te_space :: SPACE
       , _te_chars :: CHARS
       , _te_xoffs :: Int
       , _te_yoffs :: Int
       , _te_colour :: Text
       , _te_display :: Int       
       }       
  | SY { _sy_symbol :: Text
       , _sy_rot :: Maybe (Either Int Text)
       }
  | LC { _lc_linnam :: Text }
  | LS { _ls_pstyle :: PSTYLE
       , _ls_width :: Int
       , _ls_colour :: Text
       }
  | AP { _ap_pattern :: Text }
  | AC { _ac_colour :: Text
       , _ac_transp :: Maybe Int
       }
  | CS { _cs_procname :: Text }
  deriving (Data, Typeable, Show, Eq)
makeLenses ''SymbologyCommand


parseSymbologyCommand :: Parser SymbologyCommand
parseSymbologyCommand = choice [ parseShowText, parseSymbol
                               , parseShowLine, parseShowArea
                               , parseCS ]

parseShowText :: Parser SymbologyCommand
parseShowText = choice [ parseTE, parseTX ]

parseShowLine :: Parser SymbologyCommand
parseShowLine = choice [ parseLC, parseLS ]

parseShowArea :: Parser SymbologyCommand
parseShowArea = choice [ parseAP, parseAC ]

parseI :: Parser Text
parseI = parseI' []

parseI' :: String -> Parser Text
parseI' is = do
  eof <- atEnd
  if (eof) then return $ T.pack is
  else do
    i <- anyChar
    if (i == ';' || i == '\US') then return $ T.pack is
    else parseI' (is ++ [i])
   

parseSymbology :: Parser [SymbologyCommand]
parseSymbology = fmap (map text2symb) parseInstr

text2symb :: Text -> SymbologyCommand
text2symb = either error id . parseOnly parseSymbologyCommand

parseInstr :: Parser [Text]
parseInstr = parseInstr' []
  
parseInstr' :: [Text] -> Parser [Text]
parseInstr' is = do
  i <- parseI
  eof <- atEnd
  let _is = if (T.empty == i) then is else is ++ [i]
  if(eof) then return _is
  else parseInstr' _is
    

parseCS :: Parser SymbologyCommand
parseCS = do
  _ <- string "CS("
  _procname <- fmap T.pack $ many1 $ notChar ')'
  _ <- char ')'
  return CS { _cs_procname = _procname }


parseAP :: Parser SymbologyCommand
parseAP = do
  _ <- string "AP("
  _pattern <- fmap T.pack $ many1 $ notChar ')'
  _ <- char ')'
  return AP { _ap_pattern = _pattern }

parseAC :: Parser SymbologyCommand
parseAC = do
  _ <- string "AC("
  _colour <- fmap T.pack $ many1 $ satisfy $ notInClass "),"
  sep <- anyChar
  _transp <- case sep of
              ')' -> return Nothing
              ',' -> do
                __transp <- fmap (Just . read) $ many1 $ notChar ')'
                _ <- char ')'
                return __transp
              c -> fail $ "parseAC expected ')' or ',' got: " ++ show c
  return $ AC { _ac_colour = _colour
              , _ac_transp = _transp
              }


parseLS :: Parser SymbologyCommand
parseLS = do
  _ <- string "LS("
  _pstyle <- parsePSTYLE
  _ <- char ','
  _width <- fmap (read ) $ many1 $ notChar ','
  _ <- char ','
  _colour <- fmap T.pack $ many1 $ notChar ')'
  _ <- char ')'
  return  LS { _ls_pstyle = _pstyle
             , _ls_width = _width
             , _ls_colour = _colour
             }

parseLC :: Parser SymbologyCommand
parseLC = do
  _ <- string "LC("
  _linnam <- fmap T.pack $ many1 $ notChar ')'
  _ <- char ')'
  return LC { _lc_linnam = _linnam }

parseSymbol  :: Parser SymbologyCommand
parseSymbol = do
  _ <- string "SY("
  _symbol <- fmap T.pack $ many' $ satisfy $ notInClass "),"
  sep <- anyChar
  _rot <- case sep of
    ')' -> return Nothing
    ',' -> do
      __rot <- choice [ fmap (Just . Left . read) $ many1 digit
                      , fmap (Just . Right . T.pack) $ many1 $ notChar ')'
                      ]        
      _ <- char ')'
      return __rot
    c -> fail $ "parseSymbol expected ')' or ',' got: " ++ show c
  return $ SY { _sy_symbol = _symbol
              , _sy_rot = _rot
              }


parseTX :: Parser SymbologyCommand
parseTX = do
  _ <- string "TX("
  _string <- fmap T.pack $ many' $ notChar ','
  _ <- string ","
  _hjust <- parseEnum <?> "HJUST"
  _ <- char ','
  _vjust <- parseEnum <?> "VJUST"
  _ <- char ','
  _space <- parseEnum <?> "SPACE"
  _ <- string ",'"
  _chars <- parseCHARS
  _ <- string "',"
  _xoffs <- fmap read $ many1 $ notChar ','
  _ <- string ","
  _yoffs <- fmap read $ many1 $ notChar ','
  _ <- char ','
  _colour <- fmap T.pack $ many1 $ notChar ','
  _ <- char ','
  _display <- fmap read $ many1 $ notChar ')'
  _ <- char ')'
  return $ TX { _tx_string = _string
              , _tx_hjust = _hjust
              , _tx_vjust = _vjust
              , _tx_space = _space
              , _tx_chars = _chars
              , _tx_xoffs = _xoffs
              , _tx_yoffs = _yoffs
              , _tx_colour = _colour
              , _tx_display = _display
              }

parseTE :: Parser SymbologyCommand
parseTE = do
  _ <- string "TE('"
  _format <- fmap T.pack $ many' $ notChar '\''
  _ <- string "','"
  _attribs <- fmap (T.splitOn "," . T.pack) $ many' $ notChar '\''
  _ <- string "',"
  _hjust <- parseEnum <?> "HJUST"
  _ <- char ','
  _vjust <- parseEnum <?> "VJUST"
  _ <- char ','
  _space <- parseEnum <?> "SPACE"
  _ <- string ",'"
  _chars <- parseCHARS 
  _ <- string "',"
  _xoffs <- fmap read $ many1 $ notChar ','
  _ <- string ","
  _yoffs <- fmap read $ many1 $ notChar ','
  _ <- char ','
  _colour <- fmap T.pack $ many1 $ notChar ','
  _ <- char ','
  _display <- fmap read $ many1 $ notChar ')'
  _ <- char ')'
  return $ TE { _te_format = _format
              , _te_attribs = _attribs
              , _te_hjust = _hjust
              , _te_vjust = _vjust
              , _te_space = _space
              , _te_chars = _chars
              , _te_xoffs = _xoffs
              , _te_yoffs = _yoffs
              , _te_colour = _colour
              , _te_display = _display
              }


parseEnum :: (Enum a) => Parser a
parseEnum = fmap (toEnum . read) $ many1 $ notChar ','

parsePSTYLE :: Parser PSTYLE
parsePSTYLE =
  choice [ string "SOLD" >> return SOLD
         , string "DASH" >> return DASH
         , string "DOTT" >> return DOTT
         ]

parseCHARS :: Parser CHARS
parseCHARS = do
  a <- fmap (toEnum . read . return) $ satisfy isDigit
  b <- fmap (toEnum . read . return) $ satisfy isDigit
  c <- fmap (toEnum . read . return) $ satisfy isDigit
  d <- fmap read $ count 2 $ satisfy isDigit
  return CHARS { _charsFontFamily = a
               , _charsFontWeight = b
               , _charsFontStyle = c
               , _charsBodySize = d
               }


instance Enum FontFamily where
  toEnum 1 = PlainSerif
  toEnum a = error $ "FontFamily toEnum undefined for value: " ++ show a
  fromEnum PlainSerif = 1

instance Enum FontWeight where
  toEnum 4 = Light
  toEnum 5 = Medium
  toEnum 6 = Bold
  toEnum a = error $ "FontWeight toEnum undefined for value: " ++ show a
  fromEnum Light = 4
  fromEnum Medium = 5
  fromEnum Bold = 6

instance Enum FontStyle where
  toEnum 1 = NonItallic
  toEnum a = error $ "FontStyle toEnum undefined for value: " ++ show a
  fromEnum NonItallic = 1

instance Enum HJUST where
  toEnum 1 = HCENTRE
  toEnum 2 = RIGHT
  toEnum 3 = LEFT
  toEnum a = error $ "toEnum undefined for value: " ++ show a
  fromEnum HCENTRE = 1
  fromEnum RIGHT = 2
  fromEnum LEFT = 3

instance Enum VJUST where
  toEnum 1 = BOTTOM
  toEnum 2 = VCENTRE
  toEnum 3 = TOP
  toEnum a = error $ "toEnum undefined for value: " ++ show a
  fromEnum BOTTOM = 1
  fromEnum VCENTRE = 2
  fromEnum TOP = 3

instance Enum SPACE where
  toEnum 1 = Fit
  toEnum 2 = StandardSpace
  toEnum 3 = StandardSpaceWrap
  toEnum a = error $ "toEnum undefined for value: " ++ show a
  fromEnum Fit = 1
  fromEnum StandardSpace = 2
  fromEnum StandardSpaceWrap = 3

