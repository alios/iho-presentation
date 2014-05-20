{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Types.Vector
    ( Vector2
    , VectorRecord(..) 
    , VectorInstruction (..)
    , PolygonMode (..)
    , Orientation (..)
    , parseInstructions
    , parseInstruction
    ) where

import Data.Attoparsec.Text
import Data.Int
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T

import Data.IHO.S52.Types.Module

type Vector2 = (Int16, Int16)

class (Module m) => VectorRecord m where
    vector_name :: Record m -> Text
    vector_pos :: Record m -> Vector2
    vector_box_size :: Record m -> Vector2
    vector_box_pos :: Record m -> Vector2
    vector_color_refs :: Record m -> Map Char Text
    vector_xpo :: Record m -> Text
    vector_vct :: Record m -> [[VectorInstruction]]



data VectorInstruction
    = SetPenColour Char
    | SetPenTransparency Int8
    | SetPenWidth Int16
    | PenUp Vector2
    | PenDraw Vector2
    | Circle Int16
    | PolygonMode PolygonMode
    | OutlinePolygon
    | FillPolygon
    | SymbolCall Text Orientation
      deriving (Show, Eq)

data PolygonMode
    = EnterPolygonMode
    | SubPolygon 
    | PolygonDone
      deriving (Show, Eq)
                   
data Orientation
    = Upright 
    | LastPenMove
    | Tangent
      deriving (Show, Eq)

parseInstructions :: Parser [VectorInstruction]               
parseInstructions = many' $ try parseInstruction
                    
parseInstruction :: Parser VectorInstruction
parseInstruction =
    choice [ try parsePenColour
           , try parsePenTransparency
           , try parsePenWidth
           , try parsePenUp
           , try parsePenDraw
           , try parseCircle
           , try parsePolygonMode
           , try parseOutlinePolygon
           , try parseFillPolygon
           , try parseSymbolCall
           ]

parsePenColour :: Parser VectorInstruction               
parsePenColour = parseInstruction' "SP" SetPenColour $ anyChar

parsePenTransparency :: Parser VectorInstruction
parsePenTransparency = parseInstruction' "ST" SetPenTransparency
                      (do a <- satisfy $ inClass "0123"; return (read [a]))

parsePenWidth :: Parser VectorInstruction
parsePenWidth = parseInstruction' "SW" SetPenWidth
                (fmap read $ many1 digit)
         
parsePenUp :: Parser VectorInstruction
parsePenUp = parseInstruction' "PU" PenUp parseVector2 

parsePenDraw :: Parser VectorInstruction
parsePenDraw = parseInstruction' "PD" PenDraw parseVector2 

parseCircle :: Parser VectorInstruction
parseCircle = parseInstruction' "CI" Circle (fmap read $ many1 digit)

parseOutlinePolygon :: Parser VectorInstruction
parseOutlinePolygon = do
  _ <- string "EP;" ; return OutlinePolygon

parseFillPolygon :: Parser VectorInstruction
parseFillPolygon = do
  _ <- string "FP;" ; return FillPolygon


parseSymbolCall :: Parser VectorInstruction
parseSymbolCall = do
  _ <- string "SC"
  name <- many1 . satisfy . notInClass $ ","
  _ <- char ','
  d <- digit
  _ <- char ','
  return $ SymbolCall (T.pack name) $ case d of
                               '1' -> Upright
                               '2' -> LastPenMove
                               '3' -> Tangent

parsePolygonMode = parseInstruction' "PM" PolygonMode parsePolygonMode'              
parsePolygonMode' = do
  m <- satisfy $ inClass "123"
  return $ case m of
             '1' -> EnterPolygonMode
             '2' -> SubPolygon
             '3' -> PolygonDone
              
parseVector2 :: Parser Vector2         
parseVector2 = do
  x <- many1 digit
  _ <- char ','
  y <- many1 digit
  return (read x,read y)


parseInstruction' :: Text -> (t -> VectorInstruction) -> Parser t -> Parser VectorInstruction
parseInstruction' str con argp = do
    _ <- string str
    arg <- argp
    _ <- char ';'        
    return $ con arg


t1 = "SPA;SW1;PU1208,572;PD1208,1074;"
t2 = "SPA;SW1;PU1052,721;PD1356,721;"
t3 = "SPA;SW1;PU1005,971;PD1111,1075;PD1309,1075;PD1407,974;"
t4 = "SPA;SW1;PU1418,640;PD987,1071;"
t5 = "SPA;SW2;ST0;PU603,617;PM0;PD856,617;PD856,870;PD605,870;PD603,617;PM2;FP;"

noanchor = [t5]--t1,t2,t3,t4,t5]
t' = map (aa) noanchor           
--t = map evalInstructions t'


    
aa t = either fail id (parseOnly parseInstructions t)

