{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DataAnalysis
  ( opaque, red, green, blue
  , heading, text, plot, color, dat, points, legend, add, title, table
  , DataMonad, readCsv, runDataMonad, DataMonadCtx(..)
  , Html
  )
where

import Control.Applicative
import Control.Lens ((.~))
import Control.Monad (void, liftM)
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Colour (opaque, AlphaColour)
import Data.Colour.Names (red, blue, green)
import Data.Default.Class (def)
import Data.Maybe (catMaybes)
import Data.Typeable
import Graphics.Rendering.Chart hiding (Plot, plot_legend)
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import Prelude hiding (head, tail)
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified HTMLEntities.Text

type Url = T.Text
type FileExtension = String

-- our glorious data analysis monad
data DataMonadCtx m = DataMonadCtx
  -- Yield a temporary local path along with its URL
  { dataMonad_mkTmpFile
      :: FileExtension -> (FilePath -> IO ()) -> m Url
  -- Load a file from the user's temporary directory
  , dataMonad_loadUserFile :: FilePath -> m BS.ByteString
  }

type Html = T.Text

newtype DataMonad m a = DataMonad
  { unDataMonad :: WriterT Html (ReaderT (DataMonadCtx m) m) a }
  -- careful here, we don't want to expose MonadWriter/Reader instances
  -- to the user code
  deriving (Monad, Functor, Applicative)

liftDataMonad :: (Monad m) => m a -> DataMonad m a
liftDataMonad = DataMonad . lift . lift

runDataMonad :: (Monad m) => DataMonadCtx m -> DataMonad m () -> m Html
runDataMonad ctx x = runReaderT (execWriterT (unDataMonad x)) ctx

ctx :: (Monad m) => (DataMonadCtx m -> a) -> DataMonad m a
ctx f = DataMonad $ liftM f ask

-- HTML helpers
emit :: (Monad m) => Html -> DataMonad m ()
emit html = DataMonad $ tell html

emitText :: (Monad m) => T.Text -> DataMonad m ()
emitText txt = emit $ HTMLEntities.Text.text txt

-- user DSL, use string for convenience
heading :: (Monad m) => String -> DataMonad m ()
heading title = emit "<h1>" >> emitText (T.pack title) >> emit "</h1>"

text :: (Monad m) => String -> DataMonad m ()
text inner = emit "<p>" >> emitText (T.pack inner) >> emit "</p>"

class DataValue a where
  parseDataValue :: T.Text -> a
  formatDataValue :: a -> T.Text

instance DataValue T.Text where
  parseDataValue = id
  formatDataValue = id

readSafe :: (Read a) => String -> Maybe a
readSafe s =
  case reads s of
    [(x, "")] -> Just x
    [] -> Nothing

instance DataValue Double where
  parseDataValue s =
    case readSafe (T.unpack s) of
      Just x -> x
      Nothing -> error $ "Cannot parse " ++ show s ++ " as Double"
  formatDataValue = T.pack . show

instance DataValue Int where
  parseDataValue s =
    case readSafe (T.unpack s) of
      Just x -> x
      Nothing -> error $ "Cannot parse " ++ show s ++ " as Int"
  formatDataValue = T.pack . show


data Plot a b = Plot
  { plot_color :: AlphaColour Double
  , plot_data :: [(a, b)]
  , plot_legend :: String
  }

data PlotType = LinePlot

data PlotBuilder a b = PlotBuilder
  { plotBuilder_color :: AlphaColour Double
  , plotBuilder_data :: [(a, b)]
  , plotBuilder_plots :: [Plot a b]
  , plotBuilder_plotType :: PlotType
  , plotBuilder_title :: String
  , plotBuilder_legend :: String
  }

emptyPlotBuilder = PlotBuilder
  { plotBuilder_color = opaque red
  , plotBuilder_data = []
  , plotBuilder_plots = []
  , plotBuilder_plotType = LinePlot
  , plotBuilder_title = "Unnamed plot"
  , plotBuilder_legend = "Unnamed data"
  }

points :: [(Double, Double)] -> [(Double, Double)]
points = id

title :: (MonadState (PlotBuilder a b) m) => String -> m ()
title title = modify (\b -> b { plotBuilder_title = title })

legend :: (MonadState (PlotBuilder a b) m) => String -> m ()
legend s = modify (\b -> b { plotBuilder_legend = s })

color :: (MonadState (PlotBuilder a b) m) => AlphaColour Double -> m ()
color col = modify (\b -> b { plotBuilder_color = col })

linePlot :: (MonadState (PlotBuilder a b) m) => m ()
linePlot = modify (\b -> b { plotBuilder_plotType = LinePlot })

dat :: (MonadState (PlotBuilder a b) m) => [(a, b)] -> m ()
dat dat = modify (\b -> b { plotBuilder_data = dat })

add :: (MonadState (PlotBuilder a b) m) => m ()
add = modify (\b -> b { plotBuilder_plots = newPlot b : plotBuilder_plots b })
  where
    newPlot b = Plot { plot_color = plotBuilder_color b
                     , plot_data = plotBuilder_data b
                     , plot_legend = plotBuilder_legend b
                     }

renderPlot :: (PlotValue a, PlotValue b) => PlotBuilder a b -> Renderable ()
renderPlot builder = toRenderable layout
  where
    mkPlot plot =
        plot_lines_style . line_color .~ plot_color plot
        $ plot_lines_values .~ [ plot_data plot ]
        $ plot_lines_title .~ plot_legend plot
        $ def

    layout = layout_title .~ plotBuilder_title builder
           $ layout_x_axis . laxis_override .~ axisGridHide
           $ layout_y_axis . laxis_override .~ axisGridHide
           $ layout_plots .~ map (toPlot . mkPlot) (plotBuilder_plots builder)
           $ layout_grid_last .~ False
           $ def


plot :: (Monad m, PlotValue a, PlotValue b) => State (PlotBuilder a b) () -> DataMonad m ()
plot builderSt =
  do store <- ctx dataMonad_mkTmpFile
     url <- liftDataMonad . store "png" $ \fname -> void $ renderableToFile def fname plot
     emit "<p><img src=\""
     emitText url
     emit "\" alt=\""
     emitText . T.pack $ plotBuilder_title builder
     emit "\" /></p>"
  where
    builder = execState builderSt emptyPlotBuilder
    plot = renderPlot builder

readCsv
    :: (Monad m, DataValue a, DataValue b)
    => String -> T.Text -> T.Text -> Int -> Int -> DataMonad m [(a, b)]
readCsv name linesep colsep idx1 idx2 =
  do load <- ctx dataMonad_loadUserFile
     dat <- liftM T.decodeUtf8 $ liftDataMonad $ load (name ++ ".csv")
     let lines = T.splitOn linesep dat
         col1 = extractCol idx1 lines
         col2 = extractCol idx2 lines
     return $ zip col1 col2
  where
    extractCol :: (DataValue a) => Int -> [T.Text] -> [a]
    extractCol idx lines = [ parseDataValue (fields !! idx)
                            | line <- lines
                            , let fields = T.splitOn colsep line
                            , length fields > idx ]

table
    :: (Monad m, DataValue a, DataValue b)
    => String -> String -> [(a, b)] -> DataMonad m ()
table col1 col2 dat = do
  do emit "<table><thead><tr><th>"
     emitText (T.pack col1)
     emit "</th><th>"
     emitText (T.pack col2)
     emit "</th></tr></thead><tbody>"
     forM_ dat $ \(a, b) ->
       do emit "<tr><td>"
          emitText $ formatDataValue a
          emit "</td><td>"
          emitText $ formatDataValue b
          emit "</td></tr>"
     emit "</tbody></table>"
