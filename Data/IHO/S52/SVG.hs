{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.SVG ( renderSvg, renderSvg'
                        , useSymbol, lineStyleStroke, patternFill) where

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

{-
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
         
       
-}
