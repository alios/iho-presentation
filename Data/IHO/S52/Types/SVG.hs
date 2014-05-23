{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.IHO.S52.Types.SVG where

import Data.IHO.S52.Types.Vector
import Data.IHO.S52.Types.Module

import Text.Blaze.Svg
import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes as A
import Data.Int
import Data.List
import Data.String
import qualified Data.Map as Map
import Control.Monad.State
import Data.Attoparsec.Text
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Printf
import Data.Text (Text)
import qualified Data.Text as T
import Data.IHO.S52.Types.Symbol
import Data.ByteString.Lazy (ByteString)

class VectorInterpreter i where
    data InterpreterState i :: *
    defState :: InterpreterState i
    evalI :: (VectorRecord re) => Record re -> VectorInstruction -> State (InterpreterState i) () 
    finalizeI :: (VectorRecord re) => Record re -> InterpreterState i -> InterpreterState i

svgInterpreter r is = 
    let ma = do 
          _ <- sequence $ map (evalI r) is         
          fmap (st_outputBuffer . finalizeI r) get
    in evalState ma defState
  
--runState (evalI i) defState



data SVG

data InterpreterMode = None | LineMode

instance VectorInterpreter SVG where
    data InterpreterState SVG
        = IS { st_penColour :: Char
             , st_penWidth :: Int16
             , st_penTransparency :: Int8
             , st_penPos :: Vector2
             , st_penDown :: Bool
             , st_mode :: InterpreterMode
             , st_lineBuffer :: [Vector2]
             , st_outputBuffer :: [Svg]
             } 
    defState = IS { st_penColour = ' '
                  , st_penWidth = 1
                  , st_penTransparency = 0
                  , st_penPos = (0,0)
                  , st_penDown = False
                  , st_mode = None
                  , st_lineBuffer = []
                  , st_outputBuffer = []
                  }
    finalizeI re st =
        case (st_mode st) of
          None -> st
          LineMode -> mkPolyLine re st

    evalI re (SetPenColour c) = modify (\st -> st { st_penColour = c})
    evalI re (SetPenTransparency t) = modify (\st -> st { st_penTransparency = t})
    evalI re (SetPenWidth w) = modify (\st -> st { st_penWidth = w } )
    evalI re (PenUp p) = do
      st <- get
      case (st_mode st) of
        None -> return ()
        LineMode -> modify $ mkPolyLine re
      modify (\st -> st { st_penPos = p, st_penDown = False })
    evalI re (PenDraw p) = do
      st <- get
      case (st_mode st) of
        None -> modify(\st -> st { st_penPos = p
                               , st_lineBuffer = [st_penPos st, p]
                               , st_mode = LineMode 
                               })
        LineMode -> modify(\st -> st { st_penPos = p
                                   , st_lineBuffer = st_lineBuffer st ++ [p]
                                   })


--mkPolyLine :: InterpreterState SVG -> InterpreterState SVG
mkPolyLine re st = 
    let ln = polyline ! sty ! ps ! cl
        ps = (A.points $ showV2s . st_lineBuffer $ st)
        sty = (A.style . fromString $ pw )
        pw = printf "stroke-width: %0.1fmm" (w :: Float)
        w = (0.3 * (fromInteger . toInteger . st_penWidth $ st))
        c = case (Map.lookup (st_penColour st) (vector_color_refs re)) of
              Nothing -> error $ printf "unknown penColour: %c" (st_penColour st)
              Just col -> col
        cl = (A.class_ $ fromString $ T.unpack c) 
        ob = st_outputBuffer st ++ if ((length .st_lineBuffer $ st) >= 2)
                                   then [ln] 
                                   else error "PenUp with only 1 entry in linebuffer"
    in st { st_outputBuffer =  ob, st_lineBuffer = [], st_mode = None }

showV2s :: [Vector2] -> AttributeValue
showV2s = fromString . showV2s'

showV2s' :: [Vector2] -> String
showV2s' = concat . intersperse " "  . map showV2

showV2 :: Vector2 -> String
showV2 (x,y) = concat [ show x, ",", show y]



renderR :: VectorRecord m =>
          Record m -> [ByteString]
renderR re = 
    let rm is = map renderMarkup $ svgInterpreter re $ is
    in concat $ map rm $ vector_vct re


t1_is = renderR  trec


trec :: Record Symbol
trec = SymbolEntry {symb_modn = "SY", symb_rcid = 1426, symb_stat = "NIL", symb_synm = "ACHRES51", symb_sydf = VectorDrawing, symb_sycl = 981, symb_syrw = 958, symb_syhl = 1229, symb_syvl = 1304, symb_sbxc = 1621, symb_sbxr = 1471, symb_sxpo = "area where anchoring is prohibited or restricted", symb_scrf = [('A',"CHMGF")], symb_sbtm = [], symb_svct = [[SetPenColour 'A',SetPenWidth 1,PenUp (2193,1471),PenDraw (2193,1800),PenDraw (1875,1800),PenDraw (1875,1912),PenDraw (2193,1912),PenDraw (2193,2596),PenDraw (1987,2550),PenDraw (1800,2409),PenDraw (1621,2409),PenDraw (1875,2596),PenDraw (2250,2775),PenDraw (2596,2596),PenDraw (2850,2409),PenDraw (2700,2409),PenDraw (2475,2550),PenDraw (2268,2596),PenDraw (2268,1912),PenDraw (2596,1912),PenDraw (2596,1800),PenDraw (2268,1800),PenDraw (2268,1471),PenDraw (2193,1471)],[SetPenColour 'A',SetPenWidth 3,PenUp (2703,1671),PenDraw (1701,2679)]]}



