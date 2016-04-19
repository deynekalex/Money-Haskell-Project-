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
    listItem <- query allItems GetAllItems
    list2 <- listStoreNew []
    mapM_ (\(Item r)-> listStoreAppend list2 r) listItem

    --Initialize GUI
    initGUI
    window <- windowNew
    set window [windowTitle := "Учёт расходов", windowDefaultWidth := 500, windowDefaultHeight := 400, containerBorderWidth := 10]
    --mainBox
    mainBox <- vBoxNew False 1
    containerAdd window mainBox

    --aboutDeveloper
    aboutDeveloperBtn <- buttonNewWithLabel "О разработчике"

    --descriptionLabel
    histlbl <- labelNew(Just "История расходов/доходов")

    --ListView
    treeview <- treeViewNewWithModel list2
    treeViewSetHeadersVisible treeview False
    col <- treeViewColumnNew
    rend <- cellRendererTextNew
    cellLayoutPackStart col rend False
    cellLayoutSetAttributes col rend list2 (\i -> [cellText := i])
    treeViewAppendColumn treeview col
    -- Make ability to scroll task's list
    itemsScrwin <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport itemsScrwin treeview
    scrolledWindowSetPolicy itemsScrwin PolicyAutomatic PolicyAutomatic
    --set selection
    selection <- treeViewGetSelection treeview
    treeViewColumnsAutosize treeview
    treeSelectionSetMode selection SelectionSingle

    --infoBox
    infoBox <-  hBoxNew False 1
    infoLbl <- labelNew(Just "Оставшиеся деньги")
    boxPackStart infoBox infoLbl PackNatural 0

    --DescriptionPrice
    descPriceBox <- hBoxNew False 1
    descPriceBox1 <- vBoxNew False 1
    descPriceBox2 <- vBoxNew False 1
    descPriceBox3 <- vBoxNew False 1
    descPriceBox4 <- vBoxNew False 1
    descLbl <- labelNew(Just "Описание")
    priceLbl <- labelNew(Just "Цена")
    boxPackStart descPriceBox2 descLbl PackGrow 0
    boxPackStart descPriceBox3 priceLbl PackGrow 0
    boxPackStart descPriceBox descPriceBox1 PackGrow 1
    boxPackStart descPriceBox descPriceBox2 PackGrow 20
    boxPackStart descPriceBox descPriceBox3 PackGrow 20
    boxPackStart descPriceBox descPriceBox4 PackGrow 35

    --addBox
    addBox <- hBoxNew False 2
    addBox1 <- vBoxNew False 2
    addBox2 <- vBoxNew False 2
    addBox3 <- vBoxNew False 2
    addBox4 <- vBoxNew False 2
    addBox41 <- hBoxNew False 2
    addLbl <- labelNew(Just "Новый:")
    addDescEdt <- entryNew
    addPriceEdt <- entryNew
    addConsBtn <- buttonNewWithLabel "Расход "
    addIncBtn <- buttonNewWithLabel "Доход"
    boxPackStart addBox1 addLbl PackGrow 0
    boxPackStart addBox2 addDescEdt PackGrow 0
    boxPackStart addBox3 addPriceEdt PackGrow 0
    boxPackStart addBox41 addConsBtn PackGrow 0
    boxPackStart addBox41 addIncBtn PackGrow 0
    boxPackStart addBox4 addBox41 PackGrow 0
    boxPackStart addBox addBox1 PackGrow 0
    boxPackStart addBox addBox2 PackGrow 0
    boxPackStart addBox addBox3 PackGrow 0
    boxPackStart addBox addBox4 PackGrow 0

    --editBox
    editBox <- hBoxNew False 2
    editBox1 <- vBoxNew False 2
    editBox2 <- vBoxNew False 2
    editBox3 <- vBoxNew False 2
    editBox4 <- vBoxNew False 2
    edtLbl <- labelNew(Just "Изменить:")
    editDescEdt <- entryNew
    editPriceEdt <- entryNew
    editHiddenEdt <- entryNew
    saveBtn <- buttonNewWithLabel "Сохранить"
    boxPackStart addBox1 edtLbl PackGrow 0
    boxPackStart addBox2 editDescEdt PackGrow 0
    boxPackStart addBox3 editPriceEdt PackGrow 0
    boxPackStart addBox4 saveBtn PackGrow 0
    boxPackStart editBox editBox1 PackGrow 0
    boxPackStart editBox editBox2 PackGrow 0
    boxPackStart editBox editBox3 PackGrow 0
    boxPackStart editBox editBox4 PackGrow 0

    --delButton
    delBtn <- buttonNewWithLabel "Удалить"

    --packing into mainBox
    boxPackStart mainBox aboutDeveloperBtn PackNatural 0
    boxPackStart mainBox histlbl PackNatural 0
    boxPackStart mainBox itemsScrwin PackGrow 0
    boxPackStart mainBox infoBox PackNatural 0
    boxPackStart mainBox descPriceBox PackNatural 0
    boxPackStart mainBox addBox PackNatural 0
    boxPackStart mainBox editBox PackNatural 0
    boxPackStart mainBox delBtn PackNatural 0

    onClicked addConsBtn (do
        --concat two entry's
        curText <- liftM2 (++) (liftM2 (++) (entryGetText addPriceEdt) (return " - ")) (entryGetText addDescEdt)
        text1 <- entryGetText addPriceEdt::IO String
        --check for some conditions
        Control.Monad.when (not (null curText) && all isNumber text1 && not (null text1)) $ do
            listStoreAppend list2 ("Расход - " ++ curText)
            update allItems (Insert ("Расход - " ++ curText))
            entrySetText addDescEdt ""
            entrySetText addPriceEdt "")

    onClicked addIncBtn (do
        curText <- liftM2 (++) (liftM2 (++) (entryGetText addPriceEdt) (return " - ")) (entryGetText addDescEdt)
        text1 <- entryGetText addPriceEdt::IO String
        Control.Monad.when (not (null curText) && all isNumber text1 && not (null text1)) $ do
            listStoreAppend list2 ("Доход - " ++ curText)
            update allItems (Insert ("Доход - " ++ curText))
            entrySetText addDescEdt ""
            entrySetText addPriceEdt "")

    onClicked delBtn (do
        selRows <- treeSelectionGetSelectedRows selection
        Control.Monad.unless (null selRows) $ do
            let index = head (head selRows)
            update allItems (DeleteByPos index)
            listStoreRemove list2 index
            entrySetText editDescEdt "")

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
            entrySetText editHiddenEdt zero
            entrySetText editPriceEdt first
            entrySetText editDescEdt second)

    onClicked aboutDeveloperBtn (do
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
            curText <- liftM2 (++) (entryGetText editHiddenEdt)
                (liftM2 (++) (return " - ") (liftM2 (++) (
                liftM2 (++) (entryGetText editPriceEdt) (return " - "))
                (entryGetText editDescEdt)))
            text1 <- entryGetText editPriceEdt::IO String
            Control.Monad.unless (not (not (null curText) && all isNumber text1 && not (null text1))) $ do
                listStoreSetValue list2 index curText
                update allItems (Edit index curText)
                entrySetText editDescEdt ""
                entrySetText editPriceEdt ""
                entrySetText editHiddenEdt ""))

    onDestroy window mainQuit
    onDestroy window (closeAcidState allItems)
    widgetShowAll window
    mainGUI