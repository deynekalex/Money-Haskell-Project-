module Handlers.AboutBtn where

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

aboutText = "Я Александр Дейнека, студент 4 курса КТ." ++ "\n" ++ "Это приложение несомненно облегчит вам учёт расходов."

aboutDeveloperBtnHandler = do
    windowAbout <- windowNew
    mainBoxAbout <- vBoxNew False 1
    containerAdd windowAbout mainBoxAbout
    Gtk.set windowAbout [windowTitle := "О разработчике", windowDefaultWidth := 100,
                         windowDefaultHeight := 100, containerBorderWidth := 10, windowResizable := False]
    aboutEdt <- labelNew (Just aboutText)
    image <- imageNewFromFile "res/image.gif"
    boxPackStart mainBoxAbout aboutEdt PackGrow 5
    boxPackStart mainBoxAbout image PackGrow 0
    widgetShowAll windowAbout
