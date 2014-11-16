{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S52.Preslib40 (preslib40) where

import Data.IHO.S52.Types
import Data.IHO.S52.QuasiQuote

preslib40 :: Library
preslib40 = [presLibFile|data/PresLib_e4.0.0.dai|]
