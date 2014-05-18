{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.DAI.CSS where

import Data.DAI.Types
import Data.Text.Lazy (Text)
import Text.Printf
import qualified Data.Text
import Data.Prizm.Types
import Data.Prizm.Color.CIE.XYZ
import qualified Data.Map as Map
import Data.Text.Internal.Builder
import Data.Monoid 
import Data.Maybe (fromJust)
import qualified Text.CSS.Render as CSS


cssColourMap :: ColourMap -> Text
cssColourMap m = toLazyText $ buildColourMap m `mappend` uiComponents m

buildColourMap :: ColourMap -> Builder
buildColourMap m = foldl mappend mempty $ 
                   map buildColourMapEntry $ Map.toList m

toCssColour :: CIEXYZ Double -> Data.Text.Text
toCssColour c =
    let (RGB r g b) = toRGB c
        cs  = map Data.Text.pack $ 
              [ printf "%02x" r
              , printf "%02x" g
              , printf "%02x" b
              ]
    in Data.Text.concat $ "#" : cs
buildColourMapEntry :: (Data.Text.Text, ColourMapEntry) -> Builder
buildColourMapEntry (ctok, (col, name)) = 
    let c1 = Data.Text.concat [".", ctok]
        c2 = Data.Text.concat [c1, "_bg"]
    in CSS.renderBlocks 
           [ (c1, [("color", toCssColour col)])
           , (c2, [("background-color", toCssColour col)])
           ]                     
    

uiComponents m =
    CSS.renderBlock 
           ("body", [("color", lookupColour "UINFD")
                    ,("background-color", lookupColour "UIBCK")
                    ,("border-color", lookupColour "UIBDR")
                    ]) 
           where lookupColour c = 
                     let c' = fst . fromJust $ Map.lookup c m
                     in toCssColour c'

