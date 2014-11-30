{-# LANGUAGE OverloadedStrings #-}
module Data.IHO.S52.SVG.SymbolDef where


import Data.Text (Text)
import Text.Blaze.Svg (Svg)
import Text.Blaze.Internal ((!))
import qualified Text.Blaze.Internal as SVG
import qualified Text.Blaze.Svg11 as SVG
import qualified Text.Blaze.Svg11.Attributes as A
import Data.Monoid
import Data.IHO.S52.Types
import Data.IHO.S52.SVG.Helper
import Data.IHO.S52.SVG.Renderer


-- | renders a 'Record Symbol' to a symbol definetion for later use by 'svgUseSymbol'
renderSymbolDef :: Record Symbol -> Svg 
renderSymbolDef symb =
  let (x, y) = vector_box_pos symb
      (w, h) = vector_box_size symb
      (px, py) = vector_pos symb
      lwOff = 30 * 10 -- TODO: do we relay need offset here for viewbox? (line width problem)
      _x = x - lwOff
      _y = y - lwOff
      _w = w + (2 * lwOff)
      _h = h + (2 * lwOff)
      translateA = svgTranslate (-1 * px) (-1 * py)
      idA = A.id_ $ SVG.textValue $ mconcat [ symbolPrefix, vector_name symb]
  in SVG.symbol ! idA ! svgWidth (toInteger _w) ! svgHeight (toInteger _h) ! svgViewBox _x _y _w _h ! translateA $ do
    svgDesc $ vector_xpo symb
    renderSymbol symb
     
-- | renders the vector part of a 'Record Symbol'
renderSymbol :: Record Symbol -> Svg 
renderSymbol symb = renderVectorInstructions (symb_scrf symb) (mconcat . symb_svct $ symb)


