module Utils where

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

data Item = Item{typo :: String, description :: String, price :: Int, time :: UTCTime} deriving (Typeable,Eq)
data ItemList = ItemList [Item] deriving Typeable

instance Ord Item where
    task1 <= task2 = time task1 <= time task2

--interact with db
insert :: Item -> Update ItemList ()
insert item = do
     ItemList ns <- get
     put (ItemList (ns ++ [item]))

deleteByPos :: Int -> Update ItemList ()
deleteByPos pos = do
    ItemList ns <- get
    put (ItemList (take (pos - 1) ns ++ drop (pos + 1) ns))

edit :: Int -> Item -> Update ItemList ()
edit pos item = do
    ItemList ns <- get
    put (ItemList (take (pos - 1) ns ++ [item] ++ drop (pos + 1) ns))

getItemList :: Query ItemList [Item]
getItemList = do
    ItemList ns <- ask
    return ns