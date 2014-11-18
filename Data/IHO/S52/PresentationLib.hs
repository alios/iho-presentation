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

  
class (SpatialObject obj) => PresentationLib lib obj | obj -> lib where
  lookupColour :: Text -> Text -> lib -> Maybe (CIEXYZ Double)
  lookupLineStyle :: Text -> lib -> Maybe (Record LineStyle)
  lookupSymbol :: Text -> lib -> Maybe (Record Symbol)
  lookupPattern :: Text -> lib -> Maybe (Record Pattern)
  lookupObject :: obj -> lib -> Maybe (Record LookupTable)
  renderCommand :: Record LookupTable -> obj -> SymbologyCommand -> [SymbologyCommand]
  renderObject :: lib -> obj -> Maybe [SymbologyCommand]
  renderObject lib obj = do
      tbl <- lookupObject obj lib
      return . concat . map (renderCommand tbl obj) $ lupt_inst tbl
      
  
  
instance (SpatialObject obj) => PresentationLib Library obj where
  lookupSymbol = lookupRecord lib_symb symb_synm
  lookupLineStyle = lookupRecord lib_lnst lnst_linm
  lookupPattern = lookupRecord lib_patt patt_panm
  lookupColour ctus ctok lib =
    let ts = lib_cols lib
    in fmap fst $ cols_lookup ctus ctok ts
  lookupObject obj = lookupRecord lib_lupt lupt_obcl $ objName obj

lookupRecord :: Eq b => (a -> [t]) -> (t -> b) -> b -> a -> Maybe t
lookupRecord c b a = find ((==) a . b) . c


