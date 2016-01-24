{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import Control.Monad.State
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as Model
import Data.Acid
import Data.Acid.Remote
import Control.Applicative
import Control.Monad.Reader
import Data.SafeCopy
import Network
import System.Environment
import System.Exit
import System.IO
import Data.Typeable
import qualified Data.Map as Map

import Data.Char
import Data.List.Split

data Note = Note String deriving Typeable
data AllNotes = AllNotes[Note] deriving Typeable

insert :: String -> Update AllNotes ()
insert record = do
     AllNotes ns <- get
     put (AllNotes (ns ++ [Note record]))

deleteByPos :: Int -> Update AllNotes ()
deleteByPos pos = do
    AllNotes ns <- get
    put (AllNotes ((take (pos - 1) ns) ++ (drop (pos + 1) ns)))

edit :: Int -> String -> Update AllNotes ()
edit pos record = do
    AllNotes ns <- get
    put (AllNotes ((take (pos - 1) ns) ++ [Note record] ++ (drop (pos + 1) ns)))

getAllNotes :: Query AllNotes [Note]
getAllNotes = do 
    AllNotes ns <- ask
    return ns

$(deriveSafeCopy 0 'base ''Note)
$(deriveSafeCopy 0 'base ''AllNotes)
$(makeAcidic ''AllNotes ['insert, 'deleteByPos, 'edit, 'getAllNotes])

main :: IO()
main = do
	--Read saved notes
	all <- openLocalState (AllNotes [])
	listNote <- query all GetAllNotes

	list2 <- listStoreNew []
	mapM_ (\(Note r)-> listStoreAppend list2 r) listNote

	--Initialize GUI
	initGUI
	window <- windowNew
	set window [windowTitle := "Notes", windowDefaultWidth := 500, windowDefaultHeight := 400, containerBorderWidth := 10]
	--mainBox
	mainBox <- vBoxNew False 1
	containerAdd window mainBox

	--infoBox
	infoBox <-  hBoxNew False 1
	infoLbl <- labelNew(Just "Оставшиеся деньги")
	boxPackStart infoBox infoLbl PackNatural 0

	--DescriptionPrice
	descPriceBox <- hBoxNew False 1
	descLbl <- labelNew(Just "Описание")
	priceLbl <- labelNew(Just "Цена")
	boxPackStart descPriceBox descLbl PackGrow 0
	boxPackStart descPriceBox priceLbl PackGrow 0

	--addBox
	addBox <- hBoxNew False 1
	addLbl <- labelNew(Just "NewNote:")
	addEdt <- entryNew
	addEdt1 <- entryNew
	addBtn <- buttonNewWithLabel "Add"
	boxPackStart addBox addLbl PackNatural 0
	boxPackStart addBox addEdt PackGrow 0
	boxPackStart addBox addEdt1 PackGrow 0
	boxPackStart addBox addBtn PackNatural 0

	--editBox
	editBox <- hBoxNew False 1
	edtLbl <- labelNew(Just "EditNode:")
	editEdt <- entryNew
	editEdt1 <- entryNew
	saveBtn <- buttonNewWithLabel "Save"
	boxPackStart editBox edtLbl PackNatural 0
	boxPackStart editBox editEdt PackGrow 0
	boxPackStart editBox editEdt1 PackGrow 0
	boxPackStart editBox saveBtn PackNatural 0

	--delButton
	delBtn <- buttonNewWithLabel "Delete"

	--ListView
	treeview <- treeViewNewWithModel list2
	treeViewSetHeadersVisible treeview False
	col <- treeViewColumnNew
	rend <- cellRendererTextNew
	cellLayoutPackStart col rend False
	cellLayoutSetAttributes col rend list2 (\i -> [cellText := i])
	treeViewAppendColumn treeview col
	selection <- treeViewGetSelection treeview
	treeSelectionSetMode selection SelectionSingle
	lbl <- labelNew(Just "Saved Notes")
	boxPackStart mainBox lbl PackNatural 0
	boxPackStart mainBox treeview PackGrow 0
	boxPackStart mainBox infoBox PackNatural 0
	boxPackStart mainBox descPriceBox PackNatural 0
	boxPackStart mainBox addBox PackNatural 0
	boxPackStart mainBox editBox PackNatural 0
	boxPackStart mainBox delBtn PackNatural 0
	set window [windowTitle := "Notes", windowDefaultWidth := 1000, windowDefaultHeight := 800, containerBorderWidth := 30]
	
	onClicked addBtn (do 
		curText <- liftM2 (++) (liftM2 (++) (entryGetText addEdt1) (return " - ")) (entryGetText addEdt)
		text1 <- (entryGetText addEdt1)::IO String
		if ((length curText /= 0) && (Prelude.all isNumber text1) && (length text1 > 0))
		  then do
		    listStoreAppend list2 curText
		    update all (Insert curText)
		    entrySetText addEdt ""
		    entrySetText addEdt1 ""
		  else 
		    return ()
		)

	onClicked delBtn (do 
		selRows <- treeSelectionGetSelectedRows selection
		if (null selRows)
		  then
		    return ()
		  else do
		    let index = head (head selRows)
		    update all (DeleteByPos index)
		    listStoreRemove list2 index
		    entrySetText editEdt ""
		)

	onSelectionChanged selection (do
		selRows <- treeSelectionGetSelectedRows selection
		if (null selRows) 
		  then
		    return ()
		  else do
		    let index = head (head selRows)
		    v <- listStoreGetValue list2 index
		    let first = head (splitOn "-" v)
		    let second = head (tail (splitOn "-" v))
		    entrySetText editEdt first 
		    entrySetText editEdt1 second
		)

	onClicked saveBtn (do
		selRows <- treeSelectionGetSelectedRows selection
		let index = head (head selRows)
		curText <- entryGetText editEdt
		if (length curText == 0) 
		  then
		    buttonClicked delBtn
		  else do
		  	update all (Edit index curText)
		  	listStoreSetValue list2 index curText
		  	entrySetText editEdt ""
		)

	onDestroy window mainQuit
	onDestroy window (closeAcidState all)
	widgetShowAll window
	mainGUI