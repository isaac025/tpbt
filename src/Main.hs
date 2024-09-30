{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Focus (
    FocusRing,
    focusGetCurrent,
    focusNext,
    focusPrev,
    focusRing,
    focusRingCursor,
 )
import Brick.Main (App (..), defaultMain, halt)
import Brick.Types (BrickEvent (..), CursorLocation, EventM, Widget)
import Brick.Util (fg, on)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (vCenter)
import Brick.Widgets.Core (
    Padding (..),
    hLimitPercent,
    padAll,
    padRight,
    str,
    vLimit,
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
    listInsert,
    listSelectedAttr,
    listSelectedElement,
    listSelectedFocusedAttr,
    renderList,
 )
import Control.Monad.IO.Class
import Data.Vector (Vector, empty, fromList, zip)
import Graphics.Vty.Attributes (
    black,
    brightBlue,
    brightWhite,
    brightYellow,
    defAttr,
 )
import Graphics.Vty.Input.Events (Event (..), Key (..))
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, zoom, (%=))
import Lens.Micro.TH (makeLenses)
import TPB.Monad
import TPB.Types
import Prelude hiding (zip)

data Name
    = SearchBar
    | CategorySelection
    | ResultList
    deriving (Ord, Eq, Show)

data TpbSt = TpbSt
    { _results :: Vector Result
    , _searchBar :: Editor String Name
    , _resultList :: List Name Result
    , _categorySel :: List Name Audio
    , _fRing :: FocusRing Name
    }
makeLenses ''TpbSt

initState :: TpbSt
initState =
    TpbSt
        { _results = empty
        , _searchBar = editor SearchBar (Just 1) ""
        , _resultList = list ResultList empty 1
        , _categorySel = list CategorySelection (fromList [Music .. Other]) 1
        , _fRing = focusRing [SearchBar, CategorySelection, ResultList]
        }

ui :: TpbSt -> [Widget Name]
ui tpbst = [vCenter $ padAll 1 $ searchBarUi <=> l1 <+> str " " <+> l2]
  where
    l1 = drawResultList (Just ResultList == focusGetCurrent (_fRing tpbst)) (_resultList tpbst)
    l2 = hLimitPercent 20 $ drawCatList (Just CategorySelection == focusGetCurrent (_fRing tpbst)) (_categorySel tpbst)
    drawResultList isF ls =
        withBorderStyle unicode $
            borderWithLabel (str "Results") $
                renderList drawListElement isF ls

    drawCatList isF ls =
        withBorderStyle unicode $
            borderWithLabel (str "Categories") $
                renderList drawListElement isF ls
    drawListElement isFocused ltype =
        if isFocused
            then padRight Max $ str $ " " <> show ltype
            else str $ " " <> show ltype

    searchBarUi :: Widget Name
    searchBarUi = withBorderStyle unicode $ border $ vLimit 1 $ label <+> searchBarEditor
      where
        label = str "Search: "
        searchBarEditor = renderEditor (str . unlines) True (tpbst ^. searchBar)

insertIntoList :: Int -> Result -> EventM Name TpbSt ()
insertIntoList pos el = resultList %= listInsert pos el

insertMultipleElements :: Vector Result -> EventM Name TpbSt ()
insertMultipleElements els = mapM_ (uncurry insertIntoList) (zip (fromList [1 ..]) els)

appEvents :: BrickEvent Name e -> EventM Name TpbSt ()
appEvents (VtyEvent (EvKey KEsc [])) = halt
appEvents (VtyEvent (EvKey (KChar 'q') [])) = halt
appEvents (VtyEvent (EvKey (KChar '\t') [])) = fRing %= focusNext
appEvents (VtyEvent (EvKey KBackTab [])) = fRing %= focusPrev
appEvents (VtyEvent (EvKey KEnter [])) = do
    s <- unlines . getEditContents <$> use searchBar
    c <- listSelectedElement <$> use categorySel
    case c of
        Nothing -> pure ()
        Just (_, cat) -> do
            req <- liftIO $ mkRequest s (toCat cat)
            res <- liftIO $ fetchResults req
            insertMultipleElements res
appEvents ev@(VtyEvent e) = do
    r <- use fRing
    case focusGetCurrent r of
        Just SearchBar -> zoom searchBar $ handleEditorEvent ev
        Just ResultList -> zoom resultList $ handleListEvent e
        Just CategorySelection -> zoom categorySel $ handleListEvent e
        Nothing -> pure ()
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
        ]

attrResults :: AttrName
attrResults = attrName "results"

attrCats :: AttrName
attrCats = attrName "categories"

theApp :: App TpbSt e Name
theApp =
    App
        { appDraw = ui
        , appChooseCursor = appCursor
        , appHandleEvent = appEvents
        , appStartEvent = pure ()
        , appAttrMap = const appMap
        }

main :: IO ()
main = do
    _st <- defaultMain theApp initState
    pure ()
