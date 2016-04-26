{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Char
import           Data.List.Split
import           Data.SafeCopy
import           Data.Time
import           Data.Typeable
import           Graphics.UI.Gtk      hiding (get)

data Item = Item {
    _typo        :: String,            --typo represents type of consumption/income
    _description :: String,
    _price       :: Int,
    _time        :: UTCTime            --time and date of creation
    } deriving (Typeable,Eq)

data ItemList = ItemList [Item] deriving Typeable

makeLenses ''Item
