{-# LANGUAGE TemplateHaskell    #-}
module Types where

import Control.Monad.State
import Control.Monad
import Graphics.UI.Gtk hiding (get)
import Data.Acid
import Control.Monad.Reader
import Data.SafeCopy
import Data.Char
import Data.Typeable
import Data.List.Split
import Data.Time
import Control.Lens

data Item = Item {
    _typo :: String,            --typo represents type of consumption/income
    _description :: String,
    _price :: Int,
    _time :: UTCTime            --time and date of creation
    } deriving (Typeable,Eq)

data ItemList = ItemList [Item] deriving Typeable

makeLenses ''Item