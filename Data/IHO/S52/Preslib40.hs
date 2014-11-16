{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Preslib40 (preslib40, module Data.IHO.S52.PresentationLib) where

import Data.IHO.S52.PresentationLib
import Data.IHO.S52.QuasiQuote

preslib40 :: Library
preslib40 = [presLibFile|data/PresLib_e4.0.0.dai|]
