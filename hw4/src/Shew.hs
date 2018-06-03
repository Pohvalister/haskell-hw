{-# LANGUAGE TemplateHaskell #-}
module Shew where

import Language.Haskell.TH
import qualified Data.Text as T

class Shew a where
  shew :: a -> T.Text

deriveTextDef :: Name -> Q [Dec]
deriveTextDef t = [d|instance Shew $(conT t) where
                      shew _ = T.pack "Default_Text"
                |]

deriveText :: Name -> Q [Dec]
deriveText t = [d|instance Shew $(conT t) where
                    shew x = T.pack (show x)
                |]
