{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module App where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Focus (
    FocusRing,
    focusGetCurrent,
    focusNext,
    focusPrev,
    focusRing,
    focusRingCursor,
    focusSetCurrent,
 )
import Brick.Main (App (..), defaultMain, halt)
import Brick.Types (BrickEvent (..), CursorLocation, EventM, Widget)
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core (
    Padding (..),
    emptyWidget,
    hBox,
    hLimitPercent,
    padAll,
    padRight,
    str,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
    (<=>),
 )

import Brick.Widgets.Edit (
    Editor,
    editFocusedAttr,
    editor,
    getEditContents,
    handleEditorEvent,
    renderEditor,
 )
import Brick.Widgets.List (
    List,
    handleListEvent,
    list,
    listClear,
    listSelectedAttr,
    listSelectedElement,
    listSelectedFocusedAttr,
    renderList,
 )
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Vector (Vector, empty, fromList)
import Graphics.Vty.Attributes (
    black,
    bold,
    brightBlue,
    brightWhite,
    brightYellow,
    defAttr,
    withStyle,
 )
import Graphics.Vty.Input.Events (Event (..), Key (..), Modifier (MCtrl))
import Lens.Micro ((&), (^.), _Just)
import Lens.Micro.Mtl (use, zoom, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import Prelude hiding (null)

data Name
    = SearchBar
    | CategorySelection
    | ResultList
    deriving (Ord, Eq, Show)

data TpbSt = TpbSt
    { _results :: Vector Text
    , _searchBar :: Editor String Name
    , _resultList :: List Name Text
    , _categorySel :: List Name Text
    , _fRing :: FocusRing Name
    }
makeLenses ''TpbSt

initState :: TpbSt
initState =
    TpbSt
        { _results = empty
        , _searchBar = editor SearchBar (Just 1) ""
        , _resultList = list ResultList empty 1
        , _categorySel = list CategorySelection empty 1
        , _fRing = focusRing [SearchBar, CategorySelection, ResultList]
        }

ui :: TpbSt -> [Widget Name]
ui TpbSt{..} = [vCenter $ padAll 1 $ searchBarUi <=> l1 <+> str " " <+> l2 <=> instructions]
  where
    l1 = drawList (Just ResultList == focusGetCurrent _fRing) _resultList
    l2 = hLimitPercent 20 $ drawList (Just CategorySelection == focusGetCurrent _fRing) _categorySel

    drawList isF ls =
        withBorderStyle unicode $
            borderWithLabel (str "Results") $
                renderList drawListElement isF ls

    drawListElement isFocused ltype =
        if isFocused
            then padRight Max $ str $ " " <> show ltype
            else str $ " " <> show ltype

    searchBarUi :: Widget Name
    searchBarUi = withBorderStyle unicode $ border $ vLimit 1 $ label <+> searchBarEditor
      where
        label = str "Search: "
        searchBarEditor = renderEditor (str . unlines) True _searchBar

    instructions :: Widget Name
    instructions =
        hBox
            [ drawInstruction "Enter" "Search"
            , drawInstruction "Tab" "move to next"
            , drawInstruction "/" "go to search bar"
            , drawInstruction "C-q" "quit"
            , drawInstruction "C-k" "quit"
            , drawInstruction "Esc" "quit"
            ]

    drawInstruction :: String -> String -> Widget Name
    drawInstruction key cmd =
        withAttr attrKey (str key)
            <+> str " to "
            <+> withAttr attrBold (str cmd)
            & hCenter

appEvents :: BrickEvent Name e -> EventM Name TpbSt ()
appEvents (VtyEvent (EvKey KEsc [])) = halt
appEvents (VtyEvent (EvKey (KChar 'q') [MCtrl])) = halt
appEvents (VtyEvent (EvKey (KChar 'k') [MCtrl])) = halt
appEvents (VtyEvent (EvKey (KChar '/') [])) = fRing %= focusSetCurrent SearchBar
appEvents (VtyEvent (EvKey (KChar '\t') [])) = fRing %= focusNext
appEvents (VtyEvent (EvKey KBackTab [])) = fRing %= focusPrev
appEvents ev@(VtyEvent e) = do
    r <- use fRing
    case focusGetCurrent r of
        Just SearchBar -> zoom searchBar $ handleEditorEvent ev
        Just ResultList -> zoom resultList $ handleListEvent e
        Just CategorySelection -> zoom categorySel $ handleListEvent e
        _ -> pure ()
appEvents _ = pure ()

appCursor :: TpbSt -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = focusRingCursor (^. fRing)

appMap :: AttrMap
appMap =
    attrMap
        defAttr
        [ (listSelectedFocusedAttr, black `on` brightYellow)
        , (editFocusedAttr, fg brightWhite)
        , (listSelectedAttr, fg brightWhite)
        , (attrResults, fg brightYellow)
        , (attrCats, fg brightBlue)
        , (attrKey, withStyle (fg brightYellow) bold)
        , (attrBold, withStyle (fg brightWhite) bold)
        ]

attrKey :: AttrName
attrKey = attrName "key"

attrBold :: AttrName
attrBold = attrName "bold"

attrResults :: AttrName
attrResults = attrName "results"

attrCats :: AttrName
attrCats = attrName "categories"

app :: App TpbSt e Name
app =
    App
        { appDraw = ui
        , appChooseCursor = appCursor
        , appHandleEvent = appEvents
        , appStartEvent = pure ()
        , appAttrMap = const appMap
        }

runApp :: IO ()
runApp = void $ defaultMain app initState
