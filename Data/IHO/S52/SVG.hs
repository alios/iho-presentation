{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.SVG ( renderSvg, renderSvg'
                        , useSymbol, drawLine, drawLines, patternFill) where

import Data.IHO.S52.Types
import Data.IHO.S52.SVG.SymbolDef
import Data.IHO.S52.SVG.LineStyleDef
import Data.IHO.S52.SVG.PatternDef
import Data.IHO.S52.SVG.Helper
import Data.IHO.S52.CSS
import Data.Monoid
import Text.Blaze.Svg (Svg)
import Text.Blaze.Internal ((!))
import qualified Text.Blaze.Internal as SVG
import qualified Text.Blaze.Svg11 as SVG
import qualified Text.Blaze.Svg11.Attributes as A
import Data.Text (Text)
import Data.IHO.S52.PresentationLib

renderSvg :: Maybe Text -> Library -> Svg -> Svg
renderSvg = renderSvg' $ SVG.svg

renderSvg' :: (Svg -> Svg) -> Maybe Text -> Library -> Svg -> Svg
renderSvg' outersvg cschema lib inner = do
    SVG.docType
    outersvg ! svgns ! xlinkns $ do
      renderDefs cschema lib
      inner

svgns, xlinkns :: SVG.Attribute
svgns = SVG.customAttribute "xmlns" $
        SVG.toValue ("http://www.w3.org/2000/svg" :: String)
xlinkns = SVG.customAttribute "xmlns:xlink" $
          SVG.toValue ("http://www.w3.org/1999/xlink" :: String)

renderDefs :: Maybe Text -> Library -> Svg
renderDefs cschema lib = SVG.defs $ do
  svgColourLib lib cschema
  renderSymbolDefs lib
  renderLineStyleDefs lib
  renderPatternDefs lib
  where renderSymbolDefs = mconcat . fmap renderSymbolDef . lib_symb
        renderLineStyleDefs = mconcat . fmap renderLineStyleDef . lib_lnst
        renderPatternDefs = mconcat . fmap renderPatternDef . lib_patt



data Coordinate = Coordinate Double Double

fromVector2 :: Vector2 -> Coordinate
fromVector2 (xi,yi) =
  let (x, y) = (fromIntegral xi, fromIntegral yi)
      l = sqrt ((x*x) + (y*y))
      phi = atan2 x y 
  in Coordinate l phi

toDegree :: Double -> Double
toDegree i = (i * 180) / pi 
fromDegree :: Double -> Double
fromDegree i =  (i * pi) / 180

toVector2 :: Coordinate -> Vector2
toVector2 (Coordinate l phi) =
  (round $ l * cos phi, round $ l * sin phi)

drawLine :: PresentationLib lib => lib -> Text -> Vector2 -> Vector2 -> Svg
drawLine lib ls p0 p1 = drawLines lib ls [p0,p1]

drawLines :: PresentationLib lib => lib -> Text -> [Vector2] -> Svg
drawLines lib ls ps =
  let r = maybe (error "unable to lookupLineStyle") id  $
          lookupLineStyle ls lib  
  in SVG.g $ drawLines' r 0 ps


drawLines' :: Record LineStyle  -> Double -> [Vector2] -> Svg
drawLines' lnst off (p1:p2:ps) =
  let (rsvg, roff) = drawLine' lnst off p1 p2
  in rsvg `mappend` drawLines' lnst roff ps
drawLines' _ _ _ = mempty



drawLine' :: Record LineStyle  -> Double -> Vector2 -> Vector2 -> (Svg, Double)
drawLine' lnst off p1@(x1,y1) (x2, y2) =
  let d = (x2 - x1, y2 - y1)
      lsl = fromIntegral . fst . vector_box_size $ lnst
      lsh = toInteger $ snd . vector_box_size $ lnst
      name = vector_name lnst
      (Coordinate l phi) = fromVector2 d
      _l = l - off
      rotateA = A.transform $ SVG.rotate $ toDegree phi
      (svg, roff) = drawLineSegs off name lsl _l (fromIntegral x1) (fromIntegral y1)
  in (SVG.g ! (svgWidth l) ! (svgHeight lsh) ! rotateA $ svg, roff)
     

drawLineSegs :: Double -> Text -> Double -> Double -> Double -> Double -> (Svg, Double)
drawLineSegs off name lsl l x y
  | (off /= 0 && l <= (lsl - off)) =
      (useLineStyle (round x) (round y) name ! (svgWidth l) ! (svgTranslate (-off) 0), l)
  | (off /= 0 && l > (lsl - off)) =
      let _lsl = lsl - off
          (rsvg, rl) = drawLineSegs 0 name lsl (l - _lsl) (x + _lsl) y
          lsegoff = useLineStyle (round x) (round y) name !
                    (svgWidth _lsl) ! (svgTranslate (-off) 0)
      in (lsegoff `mappend` rsvg, rl)
  | (l <= lsl) =
      (useLineStyle (round x) (round y) name ! (svgWidth l), l)
  | (l >  lsl) =
      let (rsvg, rl) = drawLineSegs off name lsl (l - lsl) (x + lsl) y
      in ((useLineStyle (round x) (round y) name) `mappend` rsvg, rl)
                
