{-# LANGUAGE TemplateHaskell #-}

module Score where

import Database.HaskellDB.CodeGen



mkDBDirectTable "Score" 
                [("Word",  [t|String|])
                ,("Count", [t|Int|])]
