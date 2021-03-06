{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Acid
import           Data.Char
import           Data.Time
import           Graphics.UI.Gtk           hiding (get)
import           Types

--orded Items sorting by add time
instance Ord Item where
    task1 <= task2 = task1^.time <= task2^.time

--for displaying treeView
instance Show Item where
    show i = i^.typo ++ " - " ++
                show (i^.price) ++ " - " ++
                i^.description ++ "(" ++
                formatTime defaultTimeLocale format (i^.time) ++ ")" where
                    format = "%d/%m/%Y (%a) %H:%M"

isValidPrice :: String  -> Bool
isValidPrice curPrice = not (null curPrice) && all isNumber curPrice

getValidPrice :: Entry -> MaybeT IO Int
getValidPrice addPriceEdt = do
    curPrice <- lift (entryGetText addPriceEdt)
    guard (isValidPrice curPrice)
    return (read curPrice)

--return 0 + [incomes] - [consumes]
getBalance :: [Item] -> Int
getBalance list = sum (map (\item -> if item^.typo == "Доход" then item^.price else (-item^.price)) list)

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