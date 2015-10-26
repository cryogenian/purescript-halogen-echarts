module Halogen.ECharts where

import Prelude
import Control.Monad.Aff (Aff())
import Data.Maybe (Maybe(..))
import DOM (DOM())
import DOM.HTML.Types (HTMLElement())
import ECharts.Chart as Ec
import ECharts.Options as Ec
import ECharts.Effects ( ECHARTS_INIT()
                       , ECHARTS_OPTION_SET()
                       , ECHARTS_DISPOSE()
                       , ECHARTS_RESIZE()
                       , ECHARTS_REFRESH()
                       , ECHARTS_CLEAR()
                       )

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P

type EChartsState =
  { option :: Maybe Ec.Option
  , chart :: Maybe Ec.EChart
  }

initialEChartsState :: EChartsState
initialEChartsState =
  { option: Nothing
  , chart: Nothing
  }

data EChartsQuery a
  = Set Ec.Option a
  | Resize a
  | Refresh a
  | Clear a
  | Dispose a
  | Init HTMLElement a
  | Quit HTMLElement a

type EChartsEffects e = ( echartInit :: ECHARTS_INIT
                        , echartSetOption :: ECHARTS_OPTION_SET
                        , echartDispose :: ECHARTS_DISPOSE
                        , echartResize :: ECHARTS_RESIZE
                        , echartRefresh :: ECHARTS_REFRESH
                        , echartClear :: ECHARTS_CLEAR
                        , dom :: DOM
                        | e)

echarts :: forall e. Component EChartsState EChartsQuery (Aff (EChartsEffects e))
echarts = component render eval

render :: EChartsState -> ComponentHTML EChartsQuery
render _ = H.div [ P.initializer \el -> action (Init el)
                 , P.finalizer \el -> action (Quit el)] [ ]

eval :: forall e. Eval EChartsQuery EChartsState EChartsQuery (Aff (EChartsEffects e))
eval (Set opts next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> do
      chart' <- liftEff' $ Ec.setOption opts true chart
      modify (const $ {chart: pure chart', option: pure opts})
  pure next
eval (Resize next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.resize chart
  pure next
eval (Refresh next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.refresh chart
  pure next
eval (Clear next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.clear chart
  pure next
eval (Dispose next) = do
  state <- get
  case state.chart of
    Nothing -> pure unit
    Just chart -> liftEff' $ Ec.dispose chart
  pure next
eval (Init el next) = do
  chart <- liftEff' $ Ec.init Nothing el
  modify (_{chart = pure chart})
  pure next
eval (Quit el next) = do
  pure next
