{-# LANGUAGE TemplateHaskell    #-}
module Handlers.AboutBtn where

import Control.Monad.State
import Control.Monad
import Graphics.UI.Gtk as Gtk hiding (get)
import Data.Acid
import Control.Monad.Reader
import Data.SafeCopy
import Data.Char
import Data.List.Split
import Data.List hiding (insert)
import Data.Time
import Types
import Control.Lens

aboutDeveloperBtnHandler = do
    windowAbout <- windowNew
    mainBoxAbout <- vBoxNew False 1
    containerAdd windowAbout mainBoxAbout
    Gtk.set windowAbout [windowTitle := "О разработчике", windowDefaultWidth := 100, windowDefaultHeight := 100, containerBorderWidth := 10, windowResizable := False]
    aboutEdt <- labelNew (Just ("Я Александр Дейнека, студент 4 курса КТ." ++ "\n" ++ "Это приложение несомненно облегчит вам учёт расходов."))
    image <- imageNewFromFile "res/image.gif"
    boxPackStart mainBoxAbout aboutEdt PackGrow 5
    boxPackStart mainBoxAbout image PackGrow 0
    widgetShowAll windowAbout