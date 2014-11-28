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



data DrawBuffer =
  DrawBuffer { _bufferStartPos :: Maybe Vector2
             , _bufferPenPos :: Vector2
             , _bufferInstructions :: [VectorInstruction]
             , _bufferPenColour :: Text
             , _bufferPenWidth :: Int16
             , _bufferFillTrans :: Int8
             } deriving (Show)
makeClassy ''DrawBuffer  

mkEmptyDrawBuffer = 
  DrawBuffer { _bufferStartPos = Nothing
             , _bufferPenPos = (0,0)
             , _bufferInstructions = mempty
             , _bufferPenColour = "none"
             , _bufferPenWidth  = 10
             , _bufferFillTrans = 0
             }
  
data RenderState r =
  RenderState { _parentRecord :: (VectorRecord r) => Record r
              , _polygonMode :: Maybe PolygonMode
              , _mainBuffer :: Maybe DrawBuffer
              , _polygonBuffer1 :: Maybe DrawBuffer 
              , _polygonBuffer2 :: Maybe DrawBuffer
              }
makeLenses ''RenderState




mkRenderState :: (VectorRecord r) => Record r -> RenderState r
mkRenderState _r =
  RenderState { _parentRecord = _r
              , _polygonMode = Nothing
              , _mainBuffer = Just mkEmptyDrawBuffer
              , _polygonBuffer1 = Nothing
              , _polygonBuffer2 = Nothing
              }


type RenderAction t = (VectorRecord r) => RWS VectorInstruction Svg (RenderState r) ()

   
evalVI :: VectorInstruction -> RenderAction ()
evalVI (SetPenColour _c) = do
  st' <-  get
  setter <- case (st' ^. polygonMode) of
        Nothing ->
          let buffer' = bufferM st' mainBuffer "mainbuffer undefined"
          in if ((buffer' ^. bufferInstructions) == mempty)
             then return mainBuffer
             else renderVerticeBuffer >> return mainBuffer
        Just EnterPolygonMode -> return polygonBuffer1
        Just SubPolygon -> return polygonBuffer2 
        Just _ -> fail "undefined RenderMode"
  st <- get      
  let cm = vector_color_refs . view parentRecord $ st
  let colour = if (_c == '@') then "none"
               else maybe (error $ "undefined colour " ++ [_c]) id $ Map.lookup _c cm
  let buffer' = bufferM st setter  "parent buffer undefined"
  let newBuffer = Just $ buffer'{ _bufferPenColour = colour }
  case (st ^. polygonMode) of
   Nothing -> modify (set mainBuffer newBuffer)
   Just EnterPolygonMode -> modify (set polygonBuffer1 newBuffer)
   Just SubPolygon -> modify (set polygonBuffer2 newBuffer)
   Just _ -> fail "undefined case"
  
  return () --modify (set setter colour)
  
evalVI (SetPenWidth w) = do
  st' <-  get
  setter <- case (st' ^. polygonMode) of
        Nothing ->
          let buffer' = bufferM st' mainBuffer "mainbuffer undefined"
          in if ((buffer' ^. bufferInstructions) == mempty)
             then return mainBuffer
             else renderVerticeBuffer >> return mainBuffer
        Just EnterPolygonMode -> return polygonBuffer1
        Just SubPolygon -> return polygonBuffer2 
        Just x -> fail "undefined RenderMode"
  st <- get      
  let buffer' = bufferM st setter  "parent buffer undefined"
  let newBuffer = Just $ buffer'{ _bufferPenWidth = w * 30 }
  case (st ^. polygonMode) of
   Nothing -> modify (set mainBuffer newBuffer)
   Just EnterPolygonMode -> modify (set polygonBuffer1 newBuffer)
   Just SubPolygon -> modify (set polygonBuffer2 newBuffer)
   Just _ -> fail "undefined case"
  
evalVI (SetPenTransparency _t) = do
  st' <-  get
  setter <- case (st' ^. polygonMode) of
        Nothing ->
          let buffer' = bufferM st' mainBuffer "mainbuffer undefined"
          in if ((buffer' ^. bufferInstructions) == mempty)
             then return mainBuffer
             else renderVerticeBuffer >> return mainBuffer
        Just EnterPolygonMode -> return polygonBuffer1
        Just SubPolygon -> return polygonBuffer2 
        Just x -> fail "undefined RenderMode"
  st <- get
  let buffer' = bufferM st setter  "parent buffer undefined"
  let newBuffer = Just $ buffer'{ _bufferFillTrans = _t }
  case (st ^. polygonMode) of
   Nothing -> modify (set mainBuffer newBuffer)
   Just EnterPolygonMode -> modify (set polygonBuffer1 newBuffer)
   Just SubPolygon -> modify (set polygonBuffer2 newBuffer)
   Just _ -> fail "undefined case"
  
evalVI i@(PenUp _) = addPenPosM i
evalVI i@(PenDraw _) = addPenPosM i
evalVI (Circle _r) = do
  st' <- get
  buffer <- case (st' ^. polygonMode) of
        Nothing ->
          let buffer' = bufferM st' mainBuffer "mainbuffer undefined"
          in if ((buffer' ^. bufferInstructions) == mempty)
                   then return (mainBuffer)
                   else renderVerticeBuffer >> return (mainBuffer)
        Just EnterPolygonMode -> return  polygonBuffer1
        Just SubPolygon -> return polygonBuffer2
        Just x -> fail "undefined RenderMode"
  st <- get
  let buffer' = bufferM st buffer "unable to locate buffer"
  let (_cx,_cy) = _bufferPenPos buffer'
      aR = A.r . SVG.toValue . toInteger $ _r
      aCX = A.cx . SVG.toValue . toInteger $ _cx
      aCY = A.cy . SVG.toValue . toInteger $ _cy
      aStrokeWidth = A.strokeWidth . SVG.toValue . toInteger $
                     _bufferPenWidth buffer'
      aFill = A.fill $ preEscapedStringValue "none"
      aClass = A.class_ . preEscapedStringValue $
               "stroke_" ++ T.unpack (buffer' ^. bufferPenColour)
  tell (SVG.circle ! aR ! aCX ! aCY ! aStrokeWidth ! aFill ! aClass)
evalVI (PolygonMode EnterPolygonMode) = do
  st <- get
  case (st ^. polygonMode) of
   Just _m -> fail $ "must not be in polygonMode: " ++ show _m
   Nothing ->
     let buffer' = bufferM st mainBuffer "main buffer undefined"
     in put $ st { _polygonBuffer1 = Just $ buffer' {
                     _bufferStartPos = Just $ buffer' ^. bufferPenPos,
                     _bufferInstructions = mempty
                     }
              , _polygonMode = Just EnterPolygonMode
              }
evalVI (PolygonMode SubPolygon) = do
  st <- get
  let buffer = case (st ^. polygonMode) of
                Nothing -> mainBuffer
                Just EnterPolygonMode -> polygonBuffer1
                Just SubPolygon -> fail "already in SubPolygonMode"
                Just _ -> fail "undefined rendering mode"
  let buffer' = bufferM st buffer "parent buffer undefined"
  put $ st { _polygonBuffer2 = Just $ buffer' {
                _bufferStartPos = Just $ buffer' ^. bufferPenPos,
                _bufferInstructions = mempty
                }
           , _polygonMode = Just SubPolygon
           }    
evalVI (PolygonMode PolygonDone) = do
  st <- get
  case (st ^. polygonMode) of
   Nothing -> fail "called PolygonDone and not in polygon mode"
   Just _m -> 
     let pBuffer1 = st ^. polygonBuffer1
     in put st { _polygonMode = Nothing }
evalVI (OutlinePolygon) = renderPolygonBuffer False
evalVI (FillPolygon) = renderPolygonBuffer True
evalVI (SymbolCall sy o) = do
  st <- get
  let buffer =
        case (st ^. polygonMode) of
         Nothing -> mainBuffer
         Just EnterPolygonMode -> polygonBuffer1
         Just SubPolygon -> polygonBuffer2
  let buffer' = bufferM st buffer "unable to read modes buffer"
  let (x,y) = buffer' ^. bufferPenPos
  tell $ useSymbol x y sy -- TODO: implement orientation handling! 

addPenPosM i = do
  st <-  get
  let (i', v) = addPenPos i  
  let buffer = case (st ^. polygonMode) of
        Nothing -> mainBuffer
        Just EnterPolygonMode -> polygonBuffer1
        Just SubPolygon -> polygonBuffer2
        Just x -> fail "undefined RenderMode"
  let buffer' = bufferM st buffer "undefined buffer" 
  let newBuffer = Just $ buffer' { _bufferInstructions =
                                      (_bufferInstructions buffer') ++ [i]
                                 , _bufferPenPos = v }
  modify (set polygonBuffer2 newBuffer)
  case (st ^. polygonMode) of
   Nothing -> modify (set mainBuffer newBuffer)
   Just EnterPolygonMode -> modify (set polygonBuffer1 newBuffer)
   Just SubPolygon -> modify (set polygonBuffer2 newBuffer)
   Just _ -> fail "undefined case"
       
addPenPos i@(PenUp v) = (i, v)
addPenPos i@(PenDraw v) = (i, v)
addPenPos i = error "addPenPos: only PenUp or PenDraw are allowed"



bufferM st buffer err = (maybe (error err) id $ st ^. buffer)

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


renderSinglePolygonBuffer :: DrawBuffer -> Bool -> RenderAction ()
renderSinglePolygonBuffer buffer _fill  = 
  let pBuffer = buffer ^. bufferInstructions
      p0 = maybe (error $ "undefined start pos in " ++ show buffer) id $ buffer ^. bufferStartPos
      pcmds = (map renderPathCmd $ (PenUp p0) : pBuffer) ++ [SVG.z]
      aPath = A.d . mkPath $ sequence pcmds >> return ()
      penColour = buffer ^. bufferPenColour
      afillOpacity = A.fillOpacity $ SVG.toValue $ toFill $ buffer ^. bufferFillTrans
      stClass =
          if (_fill) then T.concat ["fill_", penColour]
                     else T.concat ["stroke_", penColour]
      aClass = A.class_ $ SVG.toValue stClass
      aArg = if (_fill) then A.stroke $ preEscapedStringValue "none"
             else A.fill $ preEscapedStringValue "none"
  in tell $ SVG.path ! aPath ! aClass ! aArg


toFill :: (Integral i) => i -> Float
toFill i =  1 - (fromIntegral i * 0.25)

renderPolygonBuffer :: Bool -> RenderAction ()
renderPolygonBuffer _fill = do
  _s <- get
  pb1 <- tryPolygonBuffer _fill polygonBuffer1
  pb2 <- tryPolygonBuffer _fill polygonBuffer2
  _ <- if (pb1) then modify (set polygonBuffer1 Nothing) else return ()
  if (pb2) then modify (set polygonBuffer2 Nothing) else return ()


tryPolygonBuffer _fill _l = do
  _s <- get
  case (_s ^. _l) of
   Nothing -> return False
   Just buf ->
     do renderSinglePolygonBuffer buf _fill
        return True
                         
      

renderVerticeBuffer :: RenderAction ()
renderVerticeBuffer = do
  st <- get
  let buffer = bufferM st mainBuffer "main buffer undefined"
  let vb = buffer ^. bufferInstructions
      pcmds = map renderPathCmd vb
      aStrokeWidth = A.strokeWidth . SVG.toValue . toInteger $ buffer ^. bufferPenWidth
      aFillNone = A.fill $ preEscapedStringValue "none"
      aClass = A.class_ . preEscapedStringValue $
               "stroke_" ++ T.unpack (buffer ^. bufferPenColour)
      aPath = A.d . mkPath $ sequence pcmds >> return ()      
  tell $ SVG.path ! aStrokeWidth ! aFillNone ! aClass ! aPath
  let newBuffer = buffer { _bufferInstructions = mempty }
  put st { _mainBuffer = Just $ newBuffer }
  return ()


readVI :: RenderAction ()
readVI = ask >>= evalVI

renderPathCmd :: VectorInstruction -> Path
renderPathCmd (PenUp (_x, _y)) = m _x _y
renderPathCmd (PenDraw (_x, _y)) = l _x _y
renderPathCmd _c = fail $ "undefined Path Command: " ++ show _c

  






renderSvg = renderSvg' $ SVG.svg

renderSvg' outersvg cschema lib inner =
  let x = undefined
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
  in ct ! idA $ do
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
      (_px, _py) = vector_pos m
      lwOff = 30 * 10
      _x = _x_ - lwOff
      _y = _y_ - lwOff
      _w = _w_ + (2 * lwOff)
      _h = _h_ + (2 * lwOff)
      vbs = mconcat [ show _x, " ", show _y, " ", show _w, " ", show _h ]
      translateS = "translate("++ show (-1 * _px) ++ "," ++ show (-1 * _py) ++ ")"
      translateA = A.transform $ stringValue translateS
  in svg ! (A.viewbox $ SVG.toValue vbs) ! translateA !
       (A.width $ SVG.toValue . toInteger $ _w) !
       (A.height $ SVG.toValue . toInteger $ _h) 


renderLineStyleDef m =
  let svg = (renderVectorRecordDef "patt_" SVG.pattern m)
      (_x, _y) = vector_box_pos m
      (_w, _h) = vector_box_size m
      (_px, _py) = vector_pos m
      translateS = "translate("++ show (-1 * _x) ++ "," ++ show (-1 * _y) ++ ")"
      translateA = A.patterntransform $ stringValue translateS
  in  svg ! (A.patternunits $ textValue "userSpaceOnUse") !
       (A.width $ SVG.toValue . toInteger $ _w) !
       (A.height $ SVG.toValue . toInteger $ _h) ! translateA

{-
renderPatternDef m =
  let svg' = (renderVectorRecordDef "patt_" SVG.pattern m)
      (_px, _py) = vector_pos m
      _min = patt_pami m
      _min2 = _min * 2
      translateS = "translate("++ show (-1 * _px) ++ "," ++ show (-1 * _py) ++ ")"
      translateA = A.patterntransform $ stringValue translateS
      svg = svg' ! (A.patternunits $ textValue "userSpaceOnUse") 
  in case (patt_patp m) of
      LinearPattern ->
        svg ! (A.width $ SVG.toValue . toInteger $ _min) !
        (A.height $ SVG.toValue . toInteger $ _min) !
        translateA
      StaggeredPattern -> SVG.g $ do
        svg
        svg
        
-}

renderPatternDef m =
  let idA = A.id_ $ textValue $ mconcat ["patt_", vector_name m]
      unitsA = A.patternunits $ textValue "userSpaceOnUse"
      contentUnitsA = A.patterncontentunits $ textValue "userSpaceOnUse"
      _min = patt_pami m
      (_px, _py) = vector_pos m
      (_bx, _by) = vector_box_pos m
      (_bw, _bh) = vector_box_size m
      (_tw, _th) = (_bw + _min, _bh + _min)
      intA a = a . SVG.toValue . toInteger
      translateS _x _y = concat ["translate(", show _x, ",", show _y, ")"]
      translateA = A.patterntransform . stringValue $ translateS (-1 * _px) (-1 * _py)
      ps = readVIs m      
      patternDef = SVG.pattern ! idA ! unitsA ! contentUnitsA ! translateA ! (intA A.x _bx) ! (intA A.y _by)
      desc = customParent "desc" $ do SVG.toMarkup $ mconcat [vector_xpo m, "\n", T.pack $ show m]
  in case (patt_patp m) of
      LinearPattern -> patternDef !
                       (intA A.width (_tw) ) !
                       (intA A.height (_th)) !
                       (A.class_ . preEscapedStringValue $ "patt_linear") $ do
        desc
        ps        
      StaggeredPattern -> patternDef !
                          (intA A.width  (_tw * 2)) !
                          (intA A.height (_tw * 2)) !
                          (A.class_ . preEscapedStringValue $ "patt_staggerd") $ do
        desc
        ps
        ps ! (A.transform $ stringValue $ translateS _tw _th)

        
{-

renderVectorRecordDef :: VectorRecord r => Text -> (SVG.Svg -> SVG.Svg) -> Record r -> SVG.Svg
renderVectorRecordDef pref ct rec =
  let (_px, _py) = vector_pos rec
      vname = vector_name rec
      idA = A.id_ $ textValue $ mconcat [pref, vname]
  in ct ! idA $ do
       customParent "desc" $ do SVG.toMarkup $ vector_xpo rec
--       renderVectorRecordDebug rec
       readVIs rec
-}


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

