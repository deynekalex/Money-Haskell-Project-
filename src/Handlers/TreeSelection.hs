module Handlers.TreeSelection where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Char
import           Data.List            hiding (insert)
import           Data.List.Split
import           Data.SafeCopy
import           Data.Time
import           Graphics.UI.Gtk      as Gtk hiding (get)
import           Types

import           Utils

onSelectionChangedHandler itemList editPriceEdt editDescEdt selection = do
    selRows <- treeSelectionGetSelectedRows selection
    unless (null selRows) $ do
        let index = head (head selRows)
        v <- listStoreGetValue itemList index
        entrySetText editPriceEdt (show (v^.price))
        entrySetText editDescEdt (v^.description)
