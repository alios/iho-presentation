{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.Module where

import Data.Attoparsec.Text
import Data.Int
import Data.Text (Text)

class Module m where
    data Record m :: *
    module_parser :: Parser (Record m)
    module_modn :: Record m -> Text
    module_rcid :: Record m -> Int16
    module_stat :: Record m -> Text                 

data DrawingType = VectorDrawing | RasterDrawing
                deriving (Eq, Show)

parseDrawingType :: Parser DrawingType
parseDrawingType = do
  df <- satisfy $ inClass "VR"
  return $ case df of
             'V' -> VectorDrawing
             'R' -> RasterDrawing
             _ -> error "unknown DrawingType"



