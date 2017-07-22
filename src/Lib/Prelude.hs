{-
Welcome to your custom Prelude
Export here everything that should always be in your library scope
For more info on what is exported by Protolude check:
https://github.com/sdiehl/protolude/blob/master/Symbols.md
-}
module Lib.Prelude
    ( module Exports
    , module String
    ) where

import Protolude as Exports
import Data.String as String -- Because dependencies use it
