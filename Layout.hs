{-# LANGUAGE OverloadedStrings #-}
module Layout (
      getItem
    , getWidgets
    , getWrappers
  ) where

import Prelude
import System.Directory
import Language.Haskell.TH
import Control.Monad
import Data.List
import Text.Hamlet

import Settings


data LayoutQ = LayoutQ { layoutWidget :: Q Exp
                       , layoutWrapper :: Q Exp
                       , layoutName :: String
                       }

-- | Compile the layout widget.
loadWidget :: String -> Q Exp
loadWidget name = widgetFile $ "layouts/" ++ name ++ "/layout"

-- | Compile the layout wrapper.
loadWrapper :: String -> Q Exp
loadWrapper name = hamletFile $ "templates/layouts/" ++ name ++ "/wrapper.hamlet"

-- | Create the container with the layout information and the compiled wrappers
-- and widgets.
loadLayout :: String -> LayoutQ
loadLayout name =
    LayoutQ { layoutWidget = loadWidget name
            , layoutWrapper = loadWrapper name
            , layoutName = name
            }

-- | Find the available layout directories.
availableLayouts :: Q [String]
availableLayouts =
    runIO $ do
        contents <- getDirectoryContents "templates/layouts/"
        dirsEx <- filterM (doesDirectoryExist . ("templates/layouts/" ++)) contents
        let dirs = delete "." $ delete ".." $ dirsEx
        return dirs

-- | Get a list of containers for all the avaiable layouts.
getLayouts :: Q [LayoutQ]
getLayouts = availableLayouts >>= mapM (return . loadLayout)

-- | Construct a list of (name, widget) tuples from the available layouts.
getWidgets :: Q Exp
getWidgets = do
    layouts <- getLayouts
    let widgets = map (\l -> let n = layoutName l in [| (n, $(layoutWidget l)) |]) layouts
    listE widgets

-- | Construct a list of (name, wrapper) tuples from the available layouts.
getWrappers :: Q Exp
getWrappers = do
    layouts <- getLayouts
    let wrappers = map (\l -> let n = layoutName l in [| (n, $(layoutWrapper l)) |]) layouts
    listE wrappers

-- | Look for a specific layout, and return an error if it doesn't exist.
getItem :: String -> [(String, w)] -> w
getItem name allLayouts = do
    let foundLayout = filter (\(n, _) -> n == name) allLayouts
    returnLayout foundLayout
    where
        returnLayout [] = error "Could not find the specified layout."
        returnLayout l = snd $ head l
