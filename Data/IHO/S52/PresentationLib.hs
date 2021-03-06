{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.IHO.S52.PresentationLib
       ( SpatialObject (..)
       , PresentationLib(..)
       , module Data.IHO.S52.Types
       ) where

import Data.Text (Text)
import Data.IHO.S52.Types
import Data.IHO.S52.Types.Symbology
import Control.Lens
import Data.List
import Data.Prizm.Types


class SpatialObject obj where
  objName :: obj -> Text

  
class PresentationLib lib where
  lookupColour :: Text -> Text -> lib -> Maybe (CIEXYZ Double)
  lookupLineStyle :: Text -> lib -> Maybe (Record LineStyle)
  lookupSymbol :: Text -> lib -> Maybe (Record Symbol)
  lookupPattern :: Text -> lib -> Maybe (Record Pattern)
  lookupObjectClass :: Text -> lib -> Maybe (Record LookupTable)
      
  
  
instance PresentationLib Library where
  lookupSymbol = lookupRecord lib_symb symb_synm
  lookupLineStyle = lookupRecord lib_lnst lnst_linm
  lookupPattern = lookupRecord lib_patt patt_panm
  lookupColour ctus ctok lib =
    let ts = lib_cols lib
    in fmap fst $ cols_lookup ctus ctok ts
  lookupObjectClass obj = lookupRecord lib_lupt lupt_obcl $ obj

lookupRecord :: Eq b => (a -> [t]) -> (t -> b) -> b -> a -> Maybe t
lookupRecord c b a = find ((==) a . b) . c


