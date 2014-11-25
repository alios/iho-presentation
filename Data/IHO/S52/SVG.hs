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
import Text.Blaze.Svg
import qualified Text.Blaze.Svg11 as SVG
import Text.Blaze.Svg11.Attributes as A
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
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
              , _polygonBuffer1 :: (Maybe Vector2, [VectorInstruction])
              , _polygonBuffer2 :: (Maybe Vector2, [VectorInstruction])
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
              , _polygonBuffer1 = (Nothing, mempty)
              , _polygonBuffer2 = (Nothing, mempty)
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


renderSinglePolygonBuffer :: (Maybe Vector2, [VectorInstruction]) -> Bool -> 
                             RenderAction ()
renderSinglePolygonBuffer (Nothing, _) _fill  =
  fail $ "renderPolygonBuffer called on uninitailized Buffer"
renderSinglePolygonBuffer (Just p0, pBuffer) _fill  = 
  let pcmds = (map renderPathCmd $ (PenUp p0) : pBuffer) ++ [SVG.z]
      aPath = A.d . mkPath $ sequence pcmds >> return ()
  in do
    st <- get
    let penColour = st ^. pen_colour
        afillOpacity = A.fillOpacity $ SVG.toValue $ toFill $ st ^. fill_trans
        stClass =
          if (_fill) then T.concat ["fill_", penColour]
                     else T.concat ["stroke_", penColour]
        aClass = A.class_ $ SVG.toValue stClass
        aArg =
          if (_fill) then A.stroke $ preEscapedStringValue "none"
                     else A.fill $ preEscapedStringValue "none"
    tell $ SVG.path ! aPath ! aClass ! aArg


toFill :: (Integral i) => i -> Float
toFill i =  1 - (fromIntegral i * 0.25)

renderPolygonBuffer :: Bool -> RenderAction ()
renderPolygonBuffer _fill = do
  _s <- get
  tryPolygonBuffer _fill polygonBuffer1
  tryPolygonBuffer _fill polygonBuffer2
  modify (set pen_position $
          maybe (error "unable to restore PenPosition") id $
          fst $ _s ^. polygonBuffer1)

type PolygonBufferLens = Functor f => ((Maybe Vector2, [VectorInstruction]) -> f (Maybe Vector2, [VectorInstruction])) -> RenderState r0 -> f (RenderState r0)

tryPolygonBuffer :: Bool -> PolygonBufferLens -> RenderAction ()
tryPolygonBuffer _fill _l = do
  _s <- get
  case (_s ^. _l) of
   (Nothing, _) -> return ()
   buf@(Just _, _) ->
     do renderSinglePolygonBuffer buf _fill
        modify (set _l (Nothing, []))
                         
      

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

renderPathCmd :: VectorInstruction -> Path
renderPathCmd (PenUp (_x, _y)) = m _x _y
renderPathCmd (PenDraw (_x, _y)) = l _x _y
renderPathCmd _c = fail $ "undefined Path Command: " ++ show _c


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
   Nothing -> put $ st { _polygonBuffer1 = (Just $ st ^. pen_position, [])
                       , _polygonMode = Just EnterPolygonMode
                       }
evalVI (PolygonMode SubPolygon) = do
  st <- get
  put $ st { _polygonBuffer2 = (Just $ st ^. pen_position, []),
             _polygonMode = Just SubPolygon
           }
evalVI (PolygonMode PolygonDone) = do
  st <- get
  case (st ^. polygonMode) of
   Nothing -> fail "called PolygonDone and not in polygon mode"
   Just _m -> 
     let pBuffer1 = st ^. polygonBuffer1
     in put st { _polygonMode = Nothing
               , _pen_position = maybe (error "polygonBuffer1 no pos") id $ fst pBuffer1
               }
evalVI (OutlinePolygon) = renderPolygonBuffer False
evalVI (FillPolygon) = renderPolygonBuffer True
evalVI (SymbolCall sy o) = do
  (x,y) <- fmap _pen_position get
  tell $ useSymbol x y sy -- TODO: implement orientation handling! 
  



addPenPosM i = do
  st <- get
  let (i', v) = addPenPos i      
  modify (set pen_position v)
  let (o1 , pb1is) = st ^. polygonBuffer1
      (o2 , pb2is) = st ^. polygonBuffer2
  case (st ^. polygonMode) of
       Nothing -> modify (set verticeBuffer  $ (st ^. verticeBuffer) ++ [i])
       Just EnterPolygonMode -> modify (set polygonBuffer1 (o1, pb1is ++ [i]))
       Just SubPolygon ->       modify (set polygonBuffer2 (o2, pb1is ++ [i]))       
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



renderVectorRecordDef :: VectorRecord r => Text -> (SVG.Svg -> SVG.Svg) -> Record r -> SVG.Svg
renderVectorRecordDef pref ct rec =
  let (_px, _py) = vector_pos rec
      vname = vector_name rec
      idA = A.id_ $ textValue $ mconcat [pref, vname]
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
  renderPatternDefs lib
  
renderSymbolDefs lib = mconcat $ map renderSymbolDef $ lib_symb lib
renderLineStyleDefs lib = mconcat $ map renderLineStyleDef $ lib_lnst lib
renderPatternDefs lib = mconcat $ map renderPatternDef $ lib_patt lib
renderSymbolDef m =
  let svg = renderVectorRecordDef "symb_" SVG.symbol m
      (_x_, _y_) = vector_box_pos m
      (_w_, _h_) = vector_box_size m
      lwOff = 30 * 10
      _x = _x_ - lwOff
      _y = _y_ - lwOff
      _w = _w_ + (2 * lwOff)
      _h = _h_ + (2 * lwOff)
      vbs = mconcat [ show _x, " ", show _y, " ", show _w, " ", show _h ]
  in svg ! (A.viewbox $ SVG.toValue vbs) !
       (A.width $ SVG.toValue . toInteger $ _w) !
       (A.height $ SVG.toValue . toInteger $ _h) 


renderLineStyleDef = renderPatternDef
renderPatternDef m =
  let svg = (renderVectorRecordDef "patt_" SVG.pattern m)
      (_x, _y) = vector_box_pos m
      (_w, _h) = vector_box_size m
  in  svg ! (A.patternunits $ textValue "userSpaceOnUse") !
       (A.x $ SVG.toValue . toInteger $ _x) !
       (A.y $ SVG.toValue . toInteger $ _x) !
       (A.width $ SVG.toValue . toInteger $ _w) !
       (A.height $ SVG.toValue . toInteger $ _h) 
        

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
  let ref = mconcat [ "#symb_", i ]
      refA = A.xlinkHref . SVG.toValue $ ref
      xA = A.x . SVG.toValue . toInteger $ _x
      yA = A.y . SVG.toValue . toInteger $ _y      
  in SVG.use ! refA ! xA ! yA

