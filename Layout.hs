module Layout where

import Control.Monad
import Data.List
import Language.Haskell.TH
import Prelude
import System.Directory
import Text.Hamlet

import Settings

data LayoutQ = LayoutQ { layoutWidget :: Q Exp
                       , layoutWrapper :: Q Exp
                       , layoutName :: String
                       }

loadWidget :: String -> Q Exp
loadWidget name = widgetFile $ "layouts/" ++ name ++ "/layout"

loadWrapper :: String -> Q Exp
loadWrapper name = hamletFile $ "templates/layouts/" ++ name ++ "/wrapper.hamlet"

loadLayout :: String -> LayoutQ
loadLayout name =
    LayoutQ { layoutWidget = loadWidget name
            , layoutWrapper = loadWrapper name
            , layoutName = name
            }

layoutsAvailable :: Q [String]
layoutsAvailable =
    runIO $ do
        contents <- getDirectoryContents "templates/layouts/"
        dirsEx <- filterM (doesDirectoryExist . ("templates/layouts/" ++)) contents
        let dirs = delete "." $ delete ".." $ dirsEx
        return dirs

getLayouts :: Q [LayoutQ]
getLayouts = layoutsAvailable >>= mapM (return . loadLayout)

getWidgets :: Q Exp
getWidgets = do
  layouts <- getLayouts
  let widgets = map (\l -> let n = layoutName l in [| (n, $(layoutWidget l)) |]) layouts
  listE widgets

getItem :: String -> [(String, w)] -> w
getItem name =
  snd . head . filter (\(n, _) -> n == name)

getWrappers :: Q Exp
getWrappers = do
  layouts <- getLayouts
  let wrappers = map (\l -> let n = layoutName l in [| (n, $(layoutWrapper l)) |]) layouts
  listE wrappers
