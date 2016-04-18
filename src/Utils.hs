module Utils where

import Control.Monad.State
import Control.Monad
import Graphics.UI.Gtk hiding (get)
import Data.Acid
import Control.Monad.Reader
import Data.SafeCopy
import Data.Char
import Data.List.Split

data Item = Item String
type ListOfItems = [Item]
data AllItems = AllItems ListOfItems --deriving Typeable

--interact with db

insert :: String -> Update AllItems ()
insert record = do
     AllItems ns <- get
     put (AllItems (ns ++ [Item record]))

deleteByPos :: Int -> Update AllItems ()
deleteByPos pos = do
    AllItems ns <- get
    put (AllItems (take (pos - 1) ns ++ drop (pos + 1) ns))

edit :: Int -> String -> Update AllItems ()
edit pos record = do
    AllItems ns <- get
    put (AllItems (take (pos - 1) ns ++ [Item record] ++ drop (pos + 1) ns))

getAllItems :: Query AllItems [Item]
getAllItems = do
    AllItems ns <- ask
    return ns

