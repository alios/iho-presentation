{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# Language DeriveDataTypeable #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.IHO.S52.SVG where

import Data.IHO.S52.Types
--import Data.IHO.S52.Types

import Text.Blaze.Svg
import qualified Text.Blaze.Svg11 as SVG
import Text.Blaze.Svg11 ((!))
import Text.Blaze.Svg11.Attributes as A
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Monad.RWS
import Control.Lens
import qualified Data.Map as Map
import Data.IHO.S52.CSS
import Text.Blaze.Internal


type RenderAction t = (VectorRecord r) => RWS VectorInstruction Svg (RenderState r) ()

data RenderState r =
  RenderState { _parentRecord :: (VectorRecord r) => Record r
              , _pen_colour :: Text
              , _pen_width  :: Int16
              , _fill_trans :: Int8
              , _pen_position :: Vector2
              , _verticeBuffer :: [VectorInstruction]
              , _polygonMode :: Maybe PolygonMode
              , _polygonBuffer1 :: (Vector2, [VectorInstruction])
              , _polygonBuffer2 :: [VectorInstruction]
              }
makeLenses ''RenderState

mkRenderState :: (VectorRecord r) => Record r -> RenderState r
mkRenderState _r =
  RenderState { _parentRecord = _r
              , _pen_colour = "none"
              , _pen_width  = 10
              , _fill_trans = 0                             
              , _verticeBuffer = mempty
              , _pen_position = (0,0)
              , _polygonMode = Nothing
              , _polygonBuffer1 = (undefined, mempty)
              , _polygonBuffer2 = mempty
              }

readVIs :: VectorRecord r => Record r -> Svg
readVIs _r =
  let s0 = mkRenderState _r
      is = mconcat $ vector_vct _r
  in SVG.g $ readVIs' s0 is


readVIs' :: VectorRecord r => RenderState r -> [VectorInstruction] -> Svg
readVIs' st [] =
  let (_, w) = execRWS renderVerticeBuffer undefined st
  in w
readVIs' s0 (i:is) =
  let (s1, w) = execRWS readVI i s0
      res =  readVIs' s1 is
  in w `mappend` res


{-
renderBuffer :: RenderAction ()
renderBuffer = do
  st <- get
  case (st ^. polygonMode) of
   Nothing -> renderVerticeBuffer
   Just EnterPolygonMode -> renderPolygonBuffer 0
   Just SubPolygon -> renderPolygonBuffer 1
   Just pm -> fail $ "undefined polygon mode: " ++ show pm
  -} 

renderPolygonBuffer :: Bool -> RenderAction ()
renderPolygonBuffer _fill = do
  st <- get
--  let x = polygonBuffer.element i
--  let (p0, is) = maybe (error "") id $ st ^.. polygonBuffer $  at i
      
  return ()
  

renderVerticeBuffer :: RenderAction ()
renderVerticeBuffer = do
  st <- get
  let vb = st ^. verticeBuffer
      pcmds = map renderPathCmd vb
      aStrokeWidth = A.strokeWidth . SVG.toValue . toInteger $ st ^. pen_width
      aFillNone = A.fill $ preEscapedStringValue "none"
      aClass = A.class_ . preEscapedStringValue $
               "stroke_" ++ T.unpack (st ^. pen_colour)
      aPath = A.d . mkPath $ sequence pcmds >> return ()        
  tell $ SVG.path ! aStrokeWidth ! aFillNone ! aClass ! aPath
  put st { _verticeBuffer = mempty }
  return ()


readVI :: RenderAction ()
readVI = ask >>= evalVI

renderPathCmd (PenUp (x, y)) = m x y
renderPathCmd (PenDraw (x, y)) = l x y
renderPathCmd c = fail $ "undefined Path Command: " ++ show c







evalVI :: VectorInstruction -> RenderAction ()
evalVI (SetPenColour _c) = do
  cm' <-  get
  let cm = vector_color_refs . view parentRecord $ cm'
  let colour = maybe (error $ "undefined colour " ++ [_c]) id $ Map.lookup _c cm
  modify (set pen_colour colour)
evalVI (SetPenWidth w) = modify (set pen_width (w * 30))
evalVI (SetPenTransparency _t) = modify (set fill_trans _t)
evalVI i@(PenUp _) = addPenPosM i
evalVI i@(PenDraw _) = addPenPosM i
evalVI (Circle _r) = do
  st <- get
  let (_cx,_cy) = st ^. pen_position
      aR = A.r . SVG.toValue . toInteger $ _r
      aCX = A.cx . SVG.toValue . toInteger $ _cx
      aCY = A.cy . SVG.toValue . toInteger $ _cy
      aStrokeWidth = A.strokeWidth . SVG.toValue . toInteger $ st ^. pen_width
      aFill = A.fill $ preEscapedStringValue "none"
      aClass = A.class_ . preEscapedStringValue $
               "stroke_" ++ T.unpack (st ^. pen_colour)
  tell (SVG.circle ! aR ! aCX ! aCY ! aStrokeWidth ! aFill ! aClass)
evalVI (PolygonMode EnterPolygonMode) = do
  st <- get
  case (st ^. polygonMode) of
   Just _m -> fail $ "must not be in polygonMode: " ++ show _m
   Nothing -> put $ st { _polygonBuffer1 = (st ^. pen_position, [])
                       , _polygonMode = Just EnterPolygonMode
                       }
evalVI (PolygonMode SubPolygon) = do
  st <- get
  put $ st { _polygonBuffer2 = [],
             _polygonMode = Just SubPolygon
           }
evalVI (PolygonMode PolygonDone) = do
  st <- get
  case (st ^. polygonMode) of
   Nothing -> fail "called PolygonDone and not in polygon mode"
   Just _m -> 
     let pBuffer1 = st ^. polygonBuffer1
     in put st { _polygonMode = Nothing
               , _pen_position = fst pBuffer1
               }
evalVI (OutlinePolygon) = renderPolygonBuffer False
evalVI (FillPolygon) = renderPolygonBuffer True
evalVI (SymbolCall sy o) = return ()



addPenPosM i = do
  st <- get
  let (i', v) = addPenPos i      
  modify (set pen_position v)
  let (o1 , pb1is) = st ^. polygonBuffer1
      pb2is = st ^. polygonBuffer2
  case (st ^. polygonMode) of
       Nothing -> modify (set verticeBuffer  $ (st ^. verticeBuffer) ++ [i])
       Just EnterPolygonMode -> modify (set polygonBuffer1 (o1, pb1is ++ [i]))
       Just SubPolygon -> modify (set polygonBuffer2 (pb1is ++ [i]))       
addPenPos i@(PenUp v) = (i, v)
addPenPos i@(PenDraw v) = (i, v)
addPenPos i = error "addPenPos: only PenUp or PenDraw are allowed"



renderSvg = renderSvg' $ SVG.svg

renderSvg' outersvg cschema lib inner =
  let x = 1
  in do
    SVG.docType
    outersvg ! svgns ! xlinkns $ do
      renderDefs cschema lib
      inner

svgns = customAttribute "xmlns" $
        SVG.toValue ("http://www.w3.org/2000/svg" :: String)
xlinkns = customAttribute "xmlns:xlink" $
          SVG.toValue ("http://www.w3.org/1999/xlink" :: String)



renderVectorRecordDef :: VectorRecord r => (SVG.Svg -> SVG.Svg) -> Record r -> SVG.Svg
renderVectorRecordDef ct rec =
  let (_px, _py) = vector_pos rec
      vname = vector_name rec
      idA = A.id_ $ textValue vname
      translateS = "translate("++ show (-1 * _px) ++ "," ++ show (-1 * _py) ++ ")"
      translateA = A.transform $ stringValue translateS
  in ct ! idA ! translateA $ do
       customParent "desc" $ do SVG.toMarkup $ vector_xpo rec
--       renderVectorRecordDebug rec
       readVIs rec


renderDefs cschema lib = SVG.defs $ do
  svgColourLib lib cschema
  renderSymbolDefs lib
  renderLineStyleDefs lib

renderSymbolDefs lib = mconcat $ map renderSymbolDef $ lib_symb lib
renderLineStyleDefs lib = mconcat $ map renderLineStyleDef $ lib_lnst lib
renderPatternDefs lib = mconcat $ map renderPatternDef $ lib_patt lib
renderSymbolDef = renderVectorRecordDef SVG.symbol
renderLineStyleDef = renderVectorRecordDef SVG.pattern
renderPatternDef = renderVectorRecordDef SVG.pattern

renderVectorRecordDebug :: VectorRecord m => Record m -> Svg
renderVectorRecordDebug rec = 
  let (_px, _py) = vector_pos rec
      (_bx, _by) = vector_box_pos rec
      (_bw, _bh) = vector_box_size rec
      px = A.cx $ SVG.toValue $ toInteger _px 
      py = A.cy $ SVG.toValue $ toInteger _py
      boxx = A.x $ SVG.toValue $ toInteger _bx
      boxy = A.y $ SVG.toValue $ toInteger _by
      boxw = A.width $ SVG.toValue $ toInteger _bw
      boxh = A.height $ SVG.toValue $ toInteger _bh      
  in SVG.g $ do 
    SVG.rect ! boxx ! boxy ! boxw ! boxh ! fillNone ! strokeBlue
    SVG.circle ! px ! py ! (A.r $ stringValue "10") ! strokeBlack ! fillRed
  where fillNone = A.fill $ textValue "none"
        strokeBlue = A.stroke $ textValue "blue" 
        strokeBlack = A.stroke $ textValue "black"
        fillRed = A.fill $ textValue "red"
         
       
useSymbol :: Integral t => t -> t -> Text -> Svg
useSymbol _x _y i = 
  let ref = mconcat [ "#", i ]
      refA = A.xlinkHref . SVG.toValue $ ref
      xA = A.x . SVG.toValue . toInteger $ _x
      yA = A.y . SVG.toValue . toInteger $ _y      
  in SVG.use ! refA ! xA ! yA






                           
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
