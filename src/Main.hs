{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Main where
import Utils
import Control.Monad.State
import Control.Monad
import Graphics.UI.Gtk hiding (get)
import Data.Acid
import Control.Monad.Reader
import Data.SafeCopy
import Data.Char
import Data.List.Split

$(deriveSafeCopy 0 'base ''Item)
$(deriveSafeCopy 0 'base ''AllItems)
$(makeAcidic ''AllItems ['insert, 'deleteByPos, 'edit, 'getAllItems])

main :: IO()
main = do
        --Read saved notes
        allItems <- openLocalState (AllItems [])
        --openLocalState возвращает => st -> IO(AcidState st)
        listItem <- query allItems GetAllItems
        --query
        list2 <- listStoreNew []
        mapM_ (\(Item r)-> listStoreAppend list2 r) listItem
        --Initialize GUI
        initGUI
        window <- windowNew
        set window [windowTitle := "Учёт расходов", windowDefaultWidth := 500, windowDefaultHeight := 400, containerBorderWidth := 10]

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
        --boxPackStart descPriceBox descLbl PackNatural 220
        --boxPackStart descPriceBox priceLbl PackNatural 95

        --addBox
        addBox <- hBoxNew False 1
        addLbl <- labelNew(Just "Новый: ")
        addEdt <- entryNew
        addEdt1 <- entryNew
        addBtnCons <- buttonNewWithLabel "Расход "
        addBtnInc <- buttonNewWithLabel "Доход"
        boxPackStart addBox addLbl PackNatural 15
        boxPackStart addBox addEdt PackGrow 0
        boxPackStart addBox addEdt1 PackGrow 0
        boxPackStart addBox addBtnCons PackNatural 0
        boxPackStart addBox addBtnInc PackNatural 0

        --editBox

        editBox <- hBoxNew False 1
        edtLbl <- labelNew(Just "Изменить:")
        editEdt <- entryNew
        editEdt1 <- entryNew
        editEdt2 <- entryNew
        saveBtn <- buttonNewWithLabel "Сохранить"
        boxPackStart editBox edtLbl PackNatural 5
        boxPackStart editBox editEdt PackGrow 0
        boxPackStart editBox editEdt1 PackGrow 0
        boxPackStart editBox saveBtn PackNatural 16

        --delButton
        delBtn <- buttonNewWithLabel "Удалить"

        --ListView
        treeview <- treeViewNewWithModel list2
        treeViewSetHeadersVisible treeview False
        col <- treeViewColumnNew
        rend <- cellRendererTextNew
        cellLayoutPackStart col rend False
        cellLayoutSetAttributes col rend list2 (\i -> [cellText := i])
        treeViewAppendColumn treeview col
        selection <- treeViewGetSelection treeview
        treeViewColumnsAutosize treeview
        treeSelectionSetMode selection SelectionSingle
        aboutBtn <- buttonNewWithLabel "О разработчике"
        lbl <- labelNew(Just "История расходов/доходов")
        boxPackStart mainBox aboutBtn PackNatural 0
        boxPackStart mainBox lbl PackNatural 0
        boxPackStart mainBox treeview PackGrow 0
        boxPackStart mainBox descPriceBox PackNatural 0
        boxPackStart mainBox infoBox PackNatural 0
        boxPackStart mainBox addBox PackNatural 0
        boxPackStart mainBox editBox PackNatural 0
        boxPackStart mainBox delBtn PackNatural 0
        set window [windowTitle := "Учёт расходов", windowDefaultWidth := 1000, windowDefaultHeight := 800, containerBorderWidth := 30]

        onClicked addBtnCons (do
                --concat two entry's
                curText <- liftM2 (++) (liftM2 (++) (entryGetText addEdt1) (return " - ")) (entryGetText addEdt)
                text1 <- entryGetText addEdt1::IO String
                --check for some conditions
                Control.Monad.when (not (null curText) && all isNumber text1 && not (null text1)) $ do
                    listStoreAppend list2 ("Расход - " ++ curText)
                    update allItems (Insert ("Расход - " ++ curText))
                    entrySetText addEdt ""
                    entrySetText addEdt1 "")

        onClicked addBtnInc (do
                curText <- liftM2 (++) (liftM2 (++) (entryGetText addEdt1) (return " - ")) (entryGetText addEdt)
                text1 <- entryGetText addEdt1::IO String
                Control.Monad.when (not (null curText) && all isNumber text1 && not (null text1)) $ do
                    listStoreAppend list2 ("Доход - " ++ curText)
                    update allItems (Insert ("Доход - " ++ curText))
                    entrySetText addEdt ""
                    entrySetText addEdt1 "")

        onClicked delBtn (do
                selRows <- treeSelectionGetSelectedRows selection
                Control.Monad.unless (null selRows) $ do
                    let index = head (head selRows)
                    update allItems (DeleteByPos index)
                    listStoreRemove list2 index
                    entrySetText editEdt "")

        onSelectionChanged selection (do
                --get selected row
                selRows <- treeSelectionGetSelectedRows selection
                Control.Monad.unless (null selRows) $ do
                    let index = head (head selRows)
                    v <- listStoreGetValue list2 index
                    --parse
                    let zero = head (splitOn " - " v)
                    let first = splitOn " - " v!!1
                    let second = splitOn " - " v!!2
                    entrySetText editEdt2 zero
                    entrySetText editEdt1 first
                    entrySetText editEdt second)

        onClicked aboutBtn (do
                windowAbout <- windowNew
                mainBoxAbout <- vBoxNew False 1
                containerAdd windowAbout mainBoxAbout
                set windowAbout [windowTitle := "О разработчике", windowDefaultWidth := 500, windowDefaultHeight := 400, containerBorderWidth := 30]
                aboutEdt <- labelNew (Just "Я Александр Дейнека, студент 4 курса КТ. Это приложение несомненно облегчит вам учёт расходов.")
                boxPackStart mainBoxAbout aboutEdt PackGrow 0
                widgetShowAll windowAbout)

        onClicked saveBtn (do
                selRows <- treeSelectionGetSelectedRows selection
                Control.Monad.unless (length selRows < 1) (do
                        let index = head (head selRows)
                        curText <- liftM2 (++) (entryGetText editEdt2) 
                                (liftM2 (++) (return " - ") (liftM2 (++) (
                                liftM2 (++) (entryGetText editEdt1) (return " - "))
                                (entryGetText editEdt)))
                        text1 <- entryGetText editEdt1::IO String
                        Control.Monad.unless (not (not (null curText) && all isNumber text1 && not (null text1))) $ do
                                listStoreSetValue list2 index curText
                                update allItems (Edit index curText)
                                entrySetText editEdt ""
                                entrySetText editEdt1 ""
                                entrySetText editEdt2 ""))

        onDestroy window mainQuit
        onDestroy window (closeAcidState allItems)
        widgetShowAll window
        mainGUI