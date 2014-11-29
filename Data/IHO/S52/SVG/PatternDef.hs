{-# LANGUAGE OverloadedStrings #-}
module Data.IHO.S52.SVG.PatternDef where

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


renderPatternDef :: Record Pattern -> Svg 
renderPatternDef patt =
  let (x, y) = vector_box_pos patt
      (w, h) = vector_box_size patt
      _min = patt_pami patt
      (tw, th) = (w + _min, h + _min)
      (px, py) = vector_pos patt
      translateA = svgTranslate (-1 * px) (-1 * py)
      idA = A.id_ $ SVG.textValue $ mconcat [ patternPrefix, vector_name patt]
      unitsA = A.patternunits $ SVG.textValue "userSpaceOnUse"
      pattDef = SVG.pattern ! idA ! unitsA ! svgX x ! svgY y ! translateA
      pattDesc = svgDesc $ vector_xpo patt
      ps = renderPattern patt
  in case (patt_patp patt) of
     LinearPattern -> pattDef ! svgWidth tw ! svgHeight th $
                      do pattDesc
                         ps        
     StaggeredPattern -> pattDef ! svgWidth (tw * 2) ! svgHeight (th * 2) $ do
       pattDesc
       ps
       ps ! svgTranslate tw th



renderPattern :: Record Pattern -> Svg 
renderPattern patt = renderVectorInstructions (patt_pcrf patt) (mconcat . patt_pvct $ patt)


