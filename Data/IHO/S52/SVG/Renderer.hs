{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.SVG.Renderer (renderVectorInstructions) where

import Data.Text (Text)
import Data.Monoid
import Data.Either 
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Blaze.Svg (Svg)
import Text.Blaze.Internal ((!))
import qualified Text.Blaze.Internal as SVG
import qualified Text.Blaze.Svg11 as SVG
import qualified Text.Blaze.Svg11.Attributes as A
import Control.Monad.RWS
import Data.IHO.S52.Types
import Data.IHO.S52.SVG.Helper
import Data.Int



data RendererConfig =
  RendererConfig {
    penFactor :: Integer,
    lookupColour :: Char -> Text
    }

renderConfig :: Map Char Text -> RendererConfig
renderConfig ctbl = RendererConfig {
  penFactor = 30,
  lookupColour =
    \c -> if (c == '@') then "none"
          else maybe (error $ "lookupColor: undefined color: " ++ show c) id $
               Map.lookup c ctbl
  }

type PolygonBuffer = (Vector2, [Either VectorInstruction (Int16, Int16, Int16)])

data RendererState =
  RendererState {
    config :: RendererConfig,
    penPos :: Vector2,
    penWidth :: Integer,
    penColour :: Text,
    inPolygon :: Bool,
    fillTrans :: Double,
    lineBuffer :: [VectorInstruction],
    polygonBuffers :: [PolygonBuffer]
    }


type RenderAction = RWS VectorInstruction Svg RendererState

renderState :: RendererConfig -> RendererState
renderState cfg = RendererState {
  config = cfg,
  penPos = (0,0),
  penWidth = penFactor cfg,
  penColour = "none",
  inPolygon = False,
  fillTrans = 1,
  lineBuffer = mempty,
  polygonBuffers = mempty
  }


renderVectorInstructions :: Map Char Text -> [VectorInstruction] -> Svg
renderVectorInstructions cmap is =
  let cfg = renderConfig cmap
      s0 = renderState cfg
  in SVG.g $ renderVectorInstructions' s0 is

renderVectorInstructions' :: RendererState -> [VectorInstruction] -> Svg
renderVectorInstructions' s0 [] =
  snd $ evalRWS renderLineBuffer undefined s0
renderVectorInstructions' s0 (i:is) =
  let (s1, w) = execRWS renderVectorInstruction i s0
  in w `mappend` (renderVectorInstructions' s1 is)

renderVectorInstruction :: RenderAction ()
renderVectorInstruction = ask >>= evalVectorInstruction


evalVectorInstruction :: VectorInstruction -> RenderAction ()
evalVectorInstruction (SetPenColour c) = do
  renderLineBuffer
  setPenColour c
evalVectorInstruction (SetPenWidth w) = do
  renderLineBuffer
  setPenWidth w
evalVectorInstruction (SetPenTransparency t) = setFillTrans t
evalVectorInstruction i@(PenUp v) = do
  addBufferInstruction i
  modify (\s -> s { penPos = v } )
evalVectorInstruction i@(PenDraw v) = do
  addBufferInstruction i
  modify (\s -> s { penPos = v } )
evalVectorInstruction i@(Circle r) = do
  st <- get
  if (inPolygon st)
    then addBufferInstruction i
    else do renderLineBuffer
            let (x, y) = penPos st
            tell $ svgCircle r x y
evalVectorInstruction (PolygonMode EnterPolygonMode) = do
  st <- get
  if (inPolygon $ st)
    then fail "eval EntePolygonMode: must be in line mode"
    else createNewPolygonBuffer
evalVectorInstruction (PolygonMode SubPolygon) = createNewPolygonBuffer
evalVectorInstruction (PolygonMode PolygonDone) = do
  st <- get
  let v = fst . last . polygonBuffers $ st
  put st { penPos = v } 
evalVectorInstruction OutlinePolygon =
  renderPolygonBuffers False
evalVectorInstruction FillPolygon =
  renderPolygonBuffers True
evalVectorInstruction (SymbolCall sy o) = do
  (x,y) <- fmap penPos get
  tell $ useSymbol x y sy -- TODO: handle orientation o


createNewPolygonBuffer :: RenderAction ()
createNewPolygonBuffer = do
  st <- get
  let newPBuffers = (penPos st, mempty) : (polygonBuffers st)
  put st { polygonBuffers = newPBuffers
         , inPolygon = True
         }

addBufferInstruction :: VectorInstruction -> RenderAction ()
addBufferInstruction i = do
  st <- get
  if (inPolygon st)
    then let pbuffers = polygonBuffers st
             (v0, pb0) = head pbuffers
             _i = case i of               
               (Circle r) -> let (x, y) = penPos st in Right (r, x, y)
               inst -> Left inst
             newPBuffers = (v0, pb0 ++ [_i]) : (drop 1 pbuffers)
         in modify (\s -> s { polygonBuffers = newPBuffers})
    else let newLBuffer = (lineBuffer st) ++ [i]
         in modify (\s -> s { lineBuffer = newLBuffer})

setPenColour :: Char -> RenderAction ()
setPenColour c = 
  modify (\st -> st { penColour = (lookupColour . config $ st) c } )

setPenWidth :: Integral i => i -> RenderAction ()
setPenWidth w = 
  modify (\st -> st { penWidth = toInteger w * (penFactor . config $ st) } )
  
setFillTrans :: (Show i, Integral i) => i -> RenderAction ()
setFillTrans t
  | ((t < 0) || (t > 4)) = fail $ "setFillTrans: 0 >= t <= 4. t=" ++ show t
  | otherwise = modify (\st -> st { fillTrans = 1.0 - (0.25) } )


renderPathCmd :: VectorInstruction -> SVG.Path
renderPathCmd (PenUp (x, y)) = SVG.m x y
renderPathCmd (PenDraw (x, y)) = SVG.l x y
renderPathCmd _c = fail $ "undefined Path Command: " ++ show _c

-- | renders and clears the Line Buffer
renderLineBuffer :: RenderAction ()
renderLineBuffer = do
  st <- get
  let _is = map renderPathCmd $ lineBuffer st
      pathA = A.d . SVG.mkPath $ sequence _is >> return ()
      classA = A.class_ . SVG.textValue . mconcat $ ["stroke_", penColour st]
  tell $ SVG.path ! classA ! pathA
  put st { lineBuffer = mempty }
  return ()
  

-- | renders and clear the Polygon Buffers
renderPolygonBuffers :: Bool -> RenderAction ()
renderPolygonBuffers fill = do
  st <- get
  let pBuffers = polygonBuffers st
  _ <- sequence $ map (renderPolygonBuffer fill) pBuffers
  let v = fst . last . polygonBuffers $ st
  put st { penPos = v , polygonBuffers = mempty, inPolygon = False } 

renderPolygonBuffer :: Bool -> PolygonBuffer -> RenderAction ()
renderPolygonBuffer fill (p0, xs) = 
  let _cs = rights xs
      _is = (map renderPathCmd ((PenUp p0) : (lefts xs))) ++ [SVG.z]
      pathA = A.d . SVG.mkPath $ sequence _is >> return ()
  in do
    st <- get
    let classA = A.class_ . SVG.textValue .  mconcat $
                 if (fill)
                 then ["stroke_none fill_", penColour st]
                 else ["fill_none stroke_", penColour st]
        fillOA = A.fillOpacity . SVG.toValue . fillTrans $ st
    if (fill)
         then do tell $ SVG.path ! classA ! pathA ! fillOA
                 _ <- sequence $ map (\(r, cx, cy) ->
                                       tell $ svgCircle r cx cy ! classA ! fillOA) _cs
                 return ()
         else do tell $ SVG.path ! classA ! pathA
                 _ <- sequence $ map (\(r, cx, cy) ->
                                       tell $ svgCircle r cx cy ! classA) _cs
                 return ()
