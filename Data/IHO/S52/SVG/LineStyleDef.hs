{-# LANGUAGE OverloadedStrings #-}
module Data.IHO.S52.SVG.LineStyleDef where

import Text.Blaze.Svg (Svg)
import Text.Blaze.Internal ((!))
import qualified Text.Blaze.Internal as SVG
import qualified Text.Blaze.Svg11 as SVG
import qualified Text.Blaze.Svg11.Attributes as A
import Data.Monoid
import Data.IHO.S52.Types
import Data.IHO.S52.SVG.Helper
import Data.IHO.S52.SVG.Renderer


renderLineStyleDef :: Record LineStyle -> Svg 
renderLineStyleDef lnst =
  let (x, y) = vector_box_pos lnst
      (w, h) = vector_box_size lnst
      (px, py) = vector_pos lnst
      translateA = svgTranslate (-1 * px) (-1 * py)
      idA = A.id_ $ SVG.textValue $ mconcat [ patternPrefix, vector_name lnst]
      unitsA = A.patternunits $ SVG.textValue "userSpaceOnUse"
      pattDef = SVG.pattern ! idA ! unitsA ! svgX x ! svgY y ! translateA
      lnstDesc = svgDesc $ vector_xpo lnst
      ls = renderLineStyle lnst
  in pattDef ! svgWidth w ! svgHeight h $ do lnstDesc
                                             ls

renderLineStyle :: Record LineStyle -> Svg 
renderLineStyle lnst = renderVectorInstructions (lnst_lcrf lnst) (mconcat . lnst_lvct $ lnst)

