{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Data.Time
import           Data.Typeable

--main type in app
data Item = Item {
    _typo        :: String,            --typo represents type of consumption/income
    _description :: String,
    _price       :: Int,
    _time        :: UTCTime            --time and date of creation
    } deriving (Typeable,Eq)

data ItemList = ItemList [Item] deriving Typeable

makeLenses ''Item
