{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.CSS where

import Data.IHO.S52.Types
import Text.Printf
import qualified Data.Text
import Data.Prizm.Types
import Data.Prizm.Color.CIE.XYZ
import qualified Data.Map as Map
import Data.Text.Internal.Builder
import Data.Monoid 
import qualified Text.CSS.Render as CSS
import Text.Blaze.Svg
import qualified Text.Blaze.Svg11 as SVG
import Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Internal
import Text.Blaze
import Data.List
import Data.Text (Text)


svgColourLib :: Library -> Maybe Text -> Svg
svgColourLib lib Nothing = colourMapStyle $ (lib_cols lib) !! 0
svgColourLib lib (Just schema) =
  let rec' = find (\_r -> cols_ctus _r == schema) $ lib_cols lib
      rec = maybe (error $ "undefined colour schema " ++
                 Data.Text.unpack schema) id rec'
  in colourMapStyle rec

colourMapStyle :: Record ColourTable -> Svg
colourMapStyle rec = style_ (cols_ctus rec) . buildColourMap . cols_entries $ rec

                     
style_ :: Data.Text.Text -> Builder -> Markup                     
style_ _title child =
  let stA = A.type_ $ SVG.toValue ("text/css" :: String)
      stT = A.title $ SVG.toValue _title
  in customParent "style" ! stA ! stT $ cdata child  

cdata :: Builder -> Markup
cdata txt = preEscapedToMarkup $ mconcat [ "<![CDATA["
                                         , addition
                                         , toLazyText txt
                                         , "]]>"]

addition =
  mconcat 
    [ ".s52 { stroke-linecap: round; stroke-linejoin: round;  } "
    , ".stroke_none{stroke: none} "
    , ".fill_none{fill: none} "
    ]

buildColourMap :: ColourMap -> Builder
buildColourMap =
  mconcat . map buildColourMapEntry . Map.toList


buildColourMapEntry' :: (Data.Text.Text, ColourMapEntry) -> Data.Text.Text -> Builder
buildColourMapEntry' (ctok, (col, nm)) att = 
    let c1 = Data.Text.concat [".", att, "_", ctok]
    in CSS.renderBlock (c1, [(att, toCssColour col)]) 

buildColourMapEntry :: (Data.Text.Text, ColourMapEntry) -> Builder
buildColourMapEntry e = mconcat $ map (buildColourMapEntry' e) ["stroke", "fill"]

toCssColour :: CIEXYZ Double -> Data.Text.Text
toCssColour col =
    let (RGB _r _g _b) = toRGB col
        cs  = map Data.Text.pack $ 
              [ printf "%02x" _r
              , printf "%02x" _g
              , printf "%02x" _b
              ]
    in Data.Text.concat $ "#" : cs

       
    
{-
uiComponents m = mconcat [
                  CSS.renderBlock 
                  ("body", [("color", lookupColour "UINFD")
                           ,("background-color", lookupColour "UIBCK")
                           ,("border-color", lookupColour "UIBDR")
                           ]),
                  CSS.renderBlock
                  ("svg",  [("stroke", lookupColour "UINFD")
                           ,("fill", lookupColour "UIBCK")
                           ,("fill-opacity", "0.0")
                           ,("stroke-opacity", "1.0")
                           ])

                 ]
    where lookupColour c = 
              let c' = fst . fromJust $ Map.lookup c m
              in toCssColour c'

-}


                         
