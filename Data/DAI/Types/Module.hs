{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.DAI.Types.Module where

import Data.Attoparsec.Text

class Module m where
    data Record m :: *
    module_parser :: Parser (Record m)
