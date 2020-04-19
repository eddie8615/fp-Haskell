{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types where

import GHC.Generics (Generic)
import Control.DeepSeq

data Ingredient = Nuts | Gluten | Soy | Dairy   deriving (Show, Eq, Generic, NFData)
type Recipe = [Ingredient]
data Price = P Int                              deriving (Show, Eq, Ord, Generic, NFData)
data Cupcake = CC Price Recipe                  deriving (Show, Eq, Generic, NFData)

type Tin = [Recipe]
data Spec = And Spec Spec | Or Spec Spec | Not Spec | HasCup Int Ingredient  deriving (Show,Eq,Generic,NFData)

data Bin = L | B Bin Bin  deriving (Show,Eq, Generic, NFData)
