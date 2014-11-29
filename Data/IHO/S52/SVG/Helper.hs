{-# LANGUAGE OverloadedStrings #-}
module Data.IHO.S52.SVG.Helper where

import Data.Monoid
import Data.Text (Text)
import Text.Blaze.Svg (Svg)
import Text.Blaze.Internal ((!))
import qualified Text.Blaze.Internal as SVG
import qualified Text.Blaze.Svg11 as SVG
import qualified Text.Blaze.Svg11.Attributes as A
import Data.Monoid

--
-- SVG Attribute Helper
--

svgX :: (Integral i) => i -> SVG.Attribute
svgX = A.x . SVG.toValue . toInteger

svgY:: (Integral i) => i -> SVG.Attribute
svgY = A.y . SVG.toValue . toInteger


svgWidth :: (Integral i) => i -> SVG.Attribute
svgWidth = A.width . SVG.toValue . toInteger

svgHeight :: (Integral i) => i -> SVG.Attribute
svgHeight = A.height . SVG.toValue . toInteger 
             
svgViewBox :: (Show i, Num i) => i -> i -> i -> i -> SVG.Attribute
svgViewBox _x _y _w _h = 
  let vbs = mconcat [ show _x, " ", show _y, " ", show _w, " ", show _h ]
  in A.viewbox $ SVG.toValue vbs

svgTranslate :: (Show i, Num i) => i -> i -> SVG.Attribute
svgTranslate _x _y =       
 let translateS = mconcat [ "translate(",  show _x, ",", show _y,  ")" ]
 in A.transform $ SVG.stringValue translateS


svgDesc :: (SVG.ToMarkup m) => m -> Svg
svgDesc = SVG.customParent "desc" . SVG.toMarkup 


svgCircle :: (Integral r, Integral i) => r -> i -> i -> Svg 
svgCircle r cx cy =
  let _cx = A.cx . SVG.toValue . toInteger $ cx
      _cy = A.cy . SVG.toValue . toInteger $ cy
      _r  = A.r . SVG.toValue . toInteger $ r
  in SVG.circle ! _cx ! _cy ! _r

useSymbol :: Integral i => i -> i -> Text -> Svg
useSymbol x y i = 
  let ref = mconcat [ "#", symbolPrefix , i ]
      refA = A.xlinkHref . SVG.toValue $ ref
  in SVG.use ! refA ! svgX x ! svgY y


symbolPrefix :: Text
symbolPrefix = "symb_"

patternPrefix :: Text
patternPrefix = "patt_"
