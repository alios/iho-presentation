{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# Language DeriveDataTypeable #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.IHO.S52.SVG where

import Data.IHO.S52.Types.Vector
import Data.IHO.S52.Types.Module

import Text.Blaze.Svg
import qualified Text.Blaze.Svg11 as SVG
import Text.Blaze.Svg11 ((!))
import Text.Blaze.Svg11.Attributes as A
import Data.Int
import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Monad.RWS
import Control.Lens
import qualified Data.Map as Map





data RenderState r =
  RenderState { _parentRecord :: r
              , _pen_colour :: Text
              , _pen_width  :: Int16
              , _fill_trans :: Int8
              , _verticeBuffer :: [[VectorInstruction]]
              }
makeClassy ''RenderState

type SVGRenderer r t = (VectorRecord r) => RWS VectorInstruction (RenderState r) Svg t


addPenPosM i = do
  vb <- fmap (view verticeBuffer) get
  modify (set verticeBuffer  $ addPenPos i  vb)
  
addPenPos i@(PenUp v) = addPenPos' i
addPenPos i@(PenDraw v) = addPenPos' i
addPenPos i = error "addPenPos: only PenUp or PenDraw are allowed"

addPenPos' v [] = [[v]]
addPenPos' v ([] : _) = [[v]]
addPenPos' v (is : bs) = (v : is) : bs

lastPenPos st =
  case (view verticeBuffer st) of
   [] -> (0,0)
   ([]:_) -> (0,0)
   ((i:_):_) -> case (i) of
                 PenUp v -> v
                 PenDraw v -> v
                 ui -> error "not a path instruction in vecticeBuffer"
 


evalVI (SetPenColour c) = do
  cm <- fmap (vector_color_refs . view parentRecord ) get
  let colour = maybe (error $ "undefined colour " ++ [c]) id $ Map.lookup c cm
  modify (set pen_colour colour)
evalVI (SetPenWidth w) = modify (set pen_width w)
evalVI (SetPenTransparency t) = modify (set fill_trans t)
evalVI i@(PenUp _) = addPenPosM i
evalVI i@(PenDraw _) = do
  addPenPosM i
evalVI i@(Circle r) = do
  (x,y) <- fmap lastPenPos get
  let aR = A.r . SVG.toValue . show $ r
      aX = A.x . SVG.toValue . show $ x
      aY = A.y . SVG.toValue . show $ y
  tell (SVG.circle ! aR ! aX ! aY)
evalVI (PolygonMode m) = return ()
evalVI (OutlinePolygon) = return ()
evalVI (FillPolygon) = return ()
evalVI (SymbolCall sy o) = return ()
  
--  fmap (view pen_colour)
{-

class VectorInterpreter i where
    data InterpreterState i :: *
    defState :: InterpreterState i
    evalI :: (VectorRecord re) => Record re -> VectorInstruction -> State (InterpreterState i) () 
    finalizeI :: (VectorRecord re) => Record re -> InterpreterState i -> InterpreterState i




renderIIs :: (VectorRecord re) => Record re -> [[VectorInstruction]] -> Svg
renderIIs r iis = 
    let (_, st) = undefined --runState (sequence $ map (map (evalI r) iis) defState)
        ob = st_outputBuffer $ finalizeI r st
        outsvg = do
          _ <- sequence ob
          return ()          
    in do
      outsvg

renderIs :: (VectorRecord re) => Record re -> [VectorInstruction] -> Svg
renderIs r is = 
    let (_, st) = runState (sequence $ map (evalI r) is) defState
        ob = st_outputBuffer $ finalizeI r st
        outsvg = do
          _ <- sequence ob
          return ()          
    in do
      outsvg

-- (fromString "opacity:0;")

svgInterpreter :: VectorRecord re => Record re -> Svg
svgInterpreter r = 
    let iis = vector_vct r
        cl = class_ $ fromString . T.unpack $ vector_name r
    in g ! cl $ do
      _ <- renderIs r $ concat iis
--      _ <- sequence $ map (renderIs r) iis
      return ()


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
    defState = IS { st_penColour = undefined
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

    evalI _ (SetPenColour col) = modify (\st' -> st' { st_penColour = col})
    evalI _ (SetPenTransparency pt) = modify (\st' -> st' { st_penTransparency = pt})
    evalI _ (SetPenWidth w) = modify (\st' -> st' { st_penWidth = w } )
    evalI re (PenUp p) = do
      st <- get
      case (st_mode st) of
        None -> return ()
        LineMode -> modify $ mkPolyLine re
      modify (\st' -> st' { st_penPos = p, st_penDown = False })
    evalI _ (PenDraw p) = do
      st <- get
      case (st_mode st) of
        None -> modify(\st' -> st' { st_penPos = p
                                 , st_lineBuffer = [st_penPos st, p]
                                 , st_mode = LineMode })
        LineMode -> modify(\st' -> st' { st_penPos = p
                                     , st_lineBuffer = st_lineBuffer st ++ [p] })
    -- TODO not implemented yet                   
    evalI re (Circle rad) = modify $ mkCircle re rad
    evalI _ (PolygonMode pm) = return ()
    evalI _ (OutlinePolygon) = return ()
    evalI _ (FillPolygon) = return ()
    evalI _ (SymbolCall symb ori) = return ()


floatAttrMM2 :: Float -> AttributeValue
floatAttrMM2 = fromString . printf "%0.2fmm"
floatAttrMM1 :: Float -> AttributeValue
floatAttrMM1 = fromString . printf "%0.1fmm"


mkCircle :: VectorRecord m =>
  Record m -> Int16 -> InterpreterState SVG -> InterpreterState SVG
mkCircle re rad st' = 
    let rada = A.r $ fromString $ show rad
        (x', y') = st_penPos st'
        xa = A.cx $ fromString $ show x'
        ya = A.cy $ fromString $ show y'
        col = case (Map.lookup (st_penColour st') (vector_color_refs re)) of
              Nothing -> error $ printf "unknown penColour: %c" (st_penColour st')
              Just col' -> col'
        cl = (A.class_ $ fromString $ T.unpack col) 

        pw = strokeWidth $ fromString $ show $ 30 * st_penWidth st'

        csvg = circle ! pw ! cl ! xa ! ya ! rada 
    in st' { st_outputBuffer = csvg : st_outputBuffer st' }


mkPolyLine :: VectorRecord m =>
  Record m -> InterpreterState SVG -> InterpreterState SVG
mkPolyLine re st = 
    let ln = polyline ! pw ! ps ! cl
        ps = (A.points $ showV2s . st_lineBuffer $ st)
        pw = strokeWidth $ fromString $ show $ 30 * st_penWidth st
        col = case (Map.lookup (st_penColour st) (vector_color_refs re)) of
              Nothing -> error $ printf "unknown penColour: %c" (st_penColour st)
              Just col' -> col'
        cl = (A.class_ $ fromString $ T.unpack col) 
        ob = st_outputBuffer st ++ if ((length .st_lineBuffer $ st) >= 2)
                                   then [ln] 
                                   else error "PenUp with only 1 entry in linebuffer"
    in st { st_outputBuffer =  ob, st_lineBuffer = [], st_mode = None }

showV2s :: [Vector2] -> AttributeValue
showV2s = fromString . showV2s'

showV2s' :: [Vector2] -> String
showV2s' = concat . intersperse " "  . map showV2

showV2 :: Vector2 -> String
showV2 (vx,vy) = concat [ show vx, ",", show vy]



renderR :: VectorRecord m =>
          Record m -> ByteString
renderR re = renderMarkup $ svgInterpreter re


t1_is = renderR  trec2



trec2 = SymbolEntry {symb_modn = "SY", symb_rcid = 1425, symb_stat = "NIL", symb_synm = "ACHBRT07", symb_sydf = VectorDrawing, symb_sycl = 1264, symb_syrw = 1062, symb_syhl = 506, symb_syvl = 506, symb_sbxc = 1010, symb_sbxr = 783, symb_sxpo = "designated anchor berth for a single vessel", symb_scrf = [('A',"CHMGD")], symb_sbtm = [], symb_svct = [[SetPenColour 'A',SetPenWidth 1,PenUp (1262,783),PenDraw (1262,1004)],[SetPenColour 'A',SetPenWidth 1,PenUp (1262,1289),PenDraw (1262,1207)],[SetPenColour 'A',SetPenWidth 1,PenUp (1516,1135),PenDraw (1364,1287),PenDraw (1167,1287),PenDraw (1010,1132)],[SetPenColour 'A',SetPenWidth 1,PenUp (1261,1101),Circle 101],[SetPenColour 'A',SetPenWidth 1,PenUp (1107,937),PenDraw (1424,937)]]}

trec :: Record Symbol
trec = SymbolEntry {symb_modn = "SY", symb_rcid = 1426, symb_stat = "NIL", symb_synm = "ACHRES51", symb_sydf = VectorDrawing, symb_sycl = 981, symb_syrw = 958, symb_syhl = 1229, symb_syvl = 1304, symb_sbxc = 1621, symb_sbxr = 1471, symb_sxpo = "area where anchoring is prohibited or restricted", symb_scrf = [('A',"CHMGF")], symb_sbtm = [], symb_svct = [[SetPenColour 'A',SetPenWidth 1,PenUp (2193,1471),PenDraw (2193,1800),PenDraw (1875,1800),PenDraw (1875,1912),PenDraw (2193,1912),PenDraw (2193,2596),PenDraw (1987,2550),PenDraw (1800,2409),PenDraw (1621,2409),PenDraw (1875,2596),PenDraw (2250,2775),PenDraw (2596,2596),PenDraw (2850,2409),PenDraw (2700,2409),PenDraw (2475,2550),PenDraw (2268,2596),PenDraw (2268,1912),PenDraw (2596,1912),PenDraw (2596,1800),PenDraw (2268,1800),PenDraw (2268,1471),PenDraw (2193,1471)],[SetPenColour 'A',SetPenWidth 3,PenUp (2703,1671),PenDraw (1701,2679)]]}



-}
