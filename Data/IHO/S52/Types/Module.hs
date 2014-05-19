{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.Module where

import Data.Attoparsec.Text
import Data.Int
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

class Module m where
    data Record m :: *
    module_parser :: Parser (Record m)


type VectorInstruction = Text
type Vector2 = (Int16, Int16)

class (Module m) => VectorRecord m where
    vector_pos :: Record m -> Vector2
    vector_box_size :: Record m -> Vector2
    vector_box_pos :: Record m -> Vector2
    vector_color_refs :: Record m -> Map Char Text
    vector_xpo :: Record m -> Text
    vector_vct :: Record m -> Set [VectorInstruction]


data DrawingType = VectorDrawing | RasterDrawing
                deriving (Eq, Show)

parseDrawingType :: Parser DrawingType
parseDrawingType = do
  df <- satisfy $ inClass "VR"
  return $ case df of
             'V' -> VectorDrawing
             'R' -> RasterDrawing
             _ -> error "unknown DrawingType"
