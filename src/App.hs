module App where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.State (modify)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH

import Brick.AttrMap qualified as A
import Brick.Focus qualified as FR
import Brick.Forms qualified as F
import Brick.Main qualified as M
import Brick.Types (Widget)
import Brick.Types qualified as T
import Brick.Util (fg, on)
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (
    fill,
    hLimit,
    str,
    vBox,
    vLimit,
    withAttr,
    (<+>),
    (<=>),
 )
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List qualified as L
import Data.Vector qualified as Vec

width, height :: Int
width = 640
height = 480

data Name = SearchField | ListField
    deriving (Eq, Ord, Show)

data Env = Env
    { _search :: F.Form Text () Name
    , _results :: L.List Name Char
    , _fring :: FR.FocusRing Name
    }
makeLenses ''Env

mkForm :: Text -> F.Form Text () Name
mkForm =
    let label s w = B.borderWithLabel s $ vLimit 1 (hLimit 15 $ fill ' ') <+> w
     in F.newForm
            [ label (str "Search") F.@@= F.editTextField id SearchField (Just 1)
            ]

drawUI :: Env -> [Widget Name]
drawUI e = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur = case e ^. (results . L.listSelectedL) of
        Nothing -> str "-"
        Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ e ^. (results . L.listElementsL)
    form = hLimit 50 $ F.renderForm (e ^. search)
    box =
        B.borderWithLabel label $
            hLimit 50 $
                vLimit 25 $
                    L.renderList listDrawElement True (e ^. results)
    ui =
        C.vCenter $
            vBox
                [ C.hCenter form
                , C.hCenter box
                , str " "
                , C.hCenter $ str "Press +/- to add/remove list elements."
                , C.hCenter $ str "Press Esc to exit."
                ]

appEvent :: T.BrickEvent Name () -> T.EventM Name Env ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
    fring %= FR.focusNext
appEvent (T.VtyEvent (V.EvKey V.KBackTab [])) =
    fring %= FR.focusPrev
appEvent e = do
    r <- use fring
    case FR.focusGetCurrent r of
        Just SearchField -> zoom search $ handleSearchField e
        Just ListField -> zoom results $ handleListField e
        Nothing -> pure ()

handleSearchField :: T.BrickEvent Name () -> T.EventM Name (F.Form Text () Name) ()
handleSearchField ev = do
    f <- T.gets F.formState
    case ev of
        T.VtyEvent (V.EvKey V.KEnter []) -> do
            liftIO (putStrLn "search")
            pure ()
        _ -> do
            F.handleFormEvent ev
            modify $ F.setFieldValid (not $ T.null f) SearchField

handleListField :: T.BrickEvent Name e -> T.EventM Name (L.List Name Char) ()
handleListField (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> do
            liftIO (putStrLn "download")
            pure ()
        ev -> L.handleListEvent ev
handleListField _ = pure ()

listDrawElement :: (Show a) => Bool -> a -> Widget Name
listDrawElement sel a =
    let selStr s =
            if sel
                then withAttr L.listSelectedAttr (str $ "<" <> s <> ">")
                else str s
     in C.hCenter $ str "Item" <+> selStr (show a)

initialState :: Env
initialState =
    Env
        { _search = F.setFieldValid False SearchField $ mkForm T.empty
        , _results = L.list ListField (Vec.fromList ['a', 'b', 'c']) 1
        , _fring = FR.focusRing [SearchField, ListField]
        }

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (L.listAttr, V.white `on` V.black)
        , (L.listSelectedAttr, V.blue `on` V.black)
        , (customAttr, fg V.blue)
        , (F.invalidFormInputAttr, V.white `on` V.red)
        , (E.editAttr, V.white `on` V.black)
        , (E.editFocusedAttr, V.black `on` V.yellow)
        ]

theApp :: M.App Env () Name
theApp =
    M.App
        { M.appDraw = drawUI
        , M.appChooseCursor = FR.focusRingCursor (^. fring)
        , M.appHandleEvent = appEvent
        , M.appStartEvent = pure ()
        , M.appAttrMap = const theMap
        }

runApp :: IO ()
runApp = void $ M.defaultMain theApp initialState
