{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types
    ( module Data.IHO.S52.Types.Module
    , module Data.IHO.S52.Types.Vector
    , module Data.IHO.S52.Types.LBID
    , module Data.IHO.S52.Types.ColourTable      
    , module Data.IHO.S52.Types.LookupTable      
    , module Data.IHO.S52.Types.Pattern
    , module Data.IHO.S52.Types.Symbol    
    , module Data.IHO.S52.Types.LineStyle    
    , Library (..)
    , libraryParser
    , parseLibrary
    , parseLibraryIO
    ) where

import Data.Text (Text)
import Data.Attoparsec.Text
import Data.IHO.S52.Types.Module
import Data.IHO.S52.Types.Vector
import Data.IHO.S52.Types.LBID
import Data.IHO.S52.Types.ColourTable
import Data.IHO.S52.Types.LookupTable
import Data.IHO.S52.Types.Pattern
import Data.IHO.S52.Types.Symbol
import Data.IHO.S52.Types.LineStyle
import Data.IHO.S52.Types.Helper
import qualified Data.Text.IO as T
    
data Library = 
    Library { lib_lbid :: ! (Record LibraryId)
            , lib_cols :: ! [Record ColourTable]
            , lib_lupt :: ! [Record LookupTable]
            , lib_lnst :: ! [Record LineStyle]
            , lib_symb :: ! [Record Symbol]
            , lib_patt :: ! [Record Pattern]
            }
            deriving (Show, Eq)


data S52ModuleRecord
    = COLS (Record ColourTable)
    | LUPT (Record LookupTable)
    | LNST (Record LineStyle)
    | SYMB (Record Symbol)
    | PATT (Record Pattern)

data S52ParserState = 
    S52ParserState { cols :: [Record ColourTable]
                   , lupt :: [Record LookupTable]
                   , lnst :: [Record LineStyle]
                   , symb :: [Record Symbol]
                   , patt :: [Record Pattern]
                   }

emptyParserState :: S52ParserState
emptyParserState = S52ParserState [] [] [] [] [] 

unzipS52Module :: [S52ModuleRecord] -> S52ParserState
unzipS52Module = unzipS52Module' emptyParserState

unzipS52Module' :: S52ParserState -> [S52ModuleRecord] -> S52ParserState
unzipS52Module' st [] = st
unzipS52Module' st ((COLS m):ms) = unzipS52Module' (st { cols = m : cols st }) ms
unzipS52Module' st ((LUPT m):ms) = unzipS52Module' (st { lupt = m : lupt st }) ms
unzipS52Module' st ((LNST m):ms) = unzipS52Module' (st { lnst = m : lnst st }) ms
unzipS52Module' st ((SYMB m):ms) = unzipS52Module' (st { symb = m : symb st }) ms
unzipS52Module' st ((PATT m):ms) = unzipS52Module' (st { patt = m : patt st }) ms


parseS52Module' =
    choice 
    [ fmap COLS $ try module_parser
    , fmap LUPT $ try module_parser
    , fmap LNST $ try module_parser
    , fmap SYMB $ try module_parser
    , fmap PATT $ try module_parser
    ]

parseS52Modules :: Parser [S52ModuleRecord]
parseS52Modules = parseS52Modules' []

parseS52Modules' :: [S52ModuleRecord] -> Parser [S52ModuleRecord]
parseS52Modules' ms = do
    skipSpaces
    eof <- atEnd
    if eof then return ms
    else do
      m <- parseS52Module'
      parseS52Modules' (m:ms)


libraryParser :: Parser Library
libraryParser = do
  lbid <- module_parser
  rs <- fmap unzipS52Module parseS52Modules
  return $ Library { lib_lbid = lbid
                   , lib_cols = cols rs
                   , lib_lupt = lupt rs
                   , lib_lnst = lnst rs
                   , lib_symb = symb rs
                   , lib_patt = patt rs
                   }

parseLibrary :: Text -> Either String Library
parseLibrary = parseOnly libraryParser

parseLibraryIO :: FilePath -> IO Library
parseLibraryIO f = do
  src <- T.readFile f
  case parseLibrary src of
    Left err -> fail err
    Right res' -> return res'

