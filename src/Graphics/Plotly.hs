{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Plotly Plots in Haskell
module Graphics.Plotly ( plot, scatter', scatter, histogram, heatmap
                       , scatter3d, surface'', surface', surface
                       , parcoord'', parcoord', parcoord
                       , Script, Mode (..), Type (..), Layout (..), BarMode (..)
                       , AxisType (..), Color (..), ColorMap, ColorScale (..)
                       , Symbol (..), Marker (..), Line (..), XBins (..), Margin (..)
                       , PlotConfig (..), defaultConfig
                       ) where

import           Data.List                      (zipWith4)
import qualified Data.ByteString.Lazy     as BL
import           Graphics.Plotly.Internal
import           Graphics.Plotly.Default

-- | Scatter Plot with individual x values per trace
scatter' :: [String]  -- ^ Trace Names
         -> [[Double]] -- ^ xs
         -> [[Double]] -- ^ ys
         -> PlotConfig -- ^ Plot Config
         -> Script     -- ^ Plotly Script
scatter' ns xs ys cfg@PlotConfig{..} = toScript layout traces
  where
    z'          = []
    layout      = Just $ fromConfig cfg
    traces      = if null ns then zipWith (mt Nothing) xs ys else zipWith3 mt (map Just ns) xs ys
    mt n' x' y' = mkTrace n' (Just lineMode) (Just marker) Scatter barMode bins x' y' z'

-- | Scatter Plot with same x axis for all ys
scatter :: [String]   -- ^ Trace Names
        -> [Double]   -- ^ xs
        -> [[Double]] -- ^ ys
        -> PlotConfig -- ^ Plot Config
        -> Script     -- ^ Plotly Script
scatter ns x ys cfg = scatter' ns xs ys cfg
  where
    xs = replicate (length ys) x

-- | Histogram Plot
histogram :: [String]    -- ^ Trace Names
          -> [[Double]]  -- ^ Traces
          -> PlotConfig  -- ^ Plot Config
          -> Script      -- ^ Plotly Script
histogram ns xs cfg@PlotConfig{..} = toScript layout traces
  where
    layout = Just $ fromConfig cfg
    traces = zipWith (\ns' xs' -> mkTrace (Just ns') Nothing Nothing Histogram barMode bins xs' [] []) ns xs

-- | Heatmap Plot
heatmap :: [String]   -- ^ X Categories
        -> [String]   -- ^ Y Categories
        -> [[Double]] -- ^ Data Matrix
        -> PlotConfig -- ^ Plot Config
        -> Script     -- ^ Plotly Script
heatmap xs ys zs cfg@PlotConfig{..} = toScript layout [traceh]
  where
    layout = Just $ fromConfig cfg
    traceh = mkTraceH colorScale True True xs ys zs

-- | 3D Scatter Plot
scatter3d :: [String]   -- ^ Trace Names
          -> [[Double]] -- ^ xs
          -> [[Double]] -- ^ ys
          -> [[Double]] -- ^ zs
          -> PlotConfig -- ^ Plot Config
          -> Script     -- ^ Plotly Script
scatter3d ns xs ys zs cfg@PlotConfig{..} = toScript layout traces
  where
    layout         = Just $ fromConfig cfg
    traces         = zipWith4 mt ns xs ys zs
    mt n' x' y' z' = mkTrace (Just n') (Just lineMode) (Just marker) Scatter3D barMode bins x' y' z'

-- | 3D Surface Plot
surface :: [String]     -- ^ Trace Names
        -> [[Double]]   -- ^ xs
        -> [[Double]]   -- ^ ys
        -> [[[Double]]] -- ^ zs
        -> PlotConfig   -- ^ P
        -> Script       -- ^ Trace
surface ns xs ys zs cfg@PlotConfig{..} = toScript layout traces
  where
    layout   = Just $ fromConfig cfg
    traces   = zipWith4 mt ns xs ys zs
    mt n' x' y' z' = mkTraceS (Just n') colorScale x' y' z'

-- | 3D Surface Plot with same xs and ys for all zs
surface' :: [String]     -- ^ Trace Names
         -> [Double]     -- ^ xs
         -> [Double]     -- ys
         -> [[[Double]]] -- ^ zs
         -> PlotConfig   -- ^ Plot Config
         -> Script       -- ^ Plotly Script
surface' ns xs ys = surface ns (repeat xs) (repeat ys)

-- | 3D Surface Plot where X and Y are indices
surface'' :: [String]    -- ^ Trace Names
         -> [[[Double]]] -- ^ zs
         -> PlotConfig   -- ^ Plot Config
         -> Script       -- ^ Ploty Script
surface'' ns = surface' ns [] []

-- | Parallel Coordinate Plot with line gradient
parcoord :: [Double]     -- ^ Color column
         -> [String]     -- ^ Axis Labels
         -> [[[Double]]] -- ^ traces ( rows ( values ) )
         -> PlotConfig   -- ^ Plot Config
         -> Script       -- ^ Plotly Script
parcoord cs ns vs cfg@PlotConfig{..} = toScript layout traces
  where
    layout   = Just $ fromConfig cfg
    traces   = [mkTraceP colorScale showScale reverseScale cs ns vs]

-- | Parallel Coordinate Plot with discrete line colors
parcoord' :: [Double]     -- ^ Color column
          -> [String]     -- ^ Axis Labels
          -> [[[Double]]] -- ^ traces ( rows ( values ) )
          -> PlotConfig   -- ^ Plot Config
          -> Script       -- ^ Plotly Script
parcoord' cs ns vs cfg@PlotConfig{..} = toScript layout traces
  where
    layout   = Just $ fromConfig cfg
    traces   = [mkTraceP colorScale False False cs ns vs]

-- | Parallel Coordinate Plot without line gradient
parcoord'' :: [String]     -- ^ Axis Names
           -> [[[Double]]] -- ^ traces ( rows ( values ) )
           -> PlotConfig   -- ^ Plot Config
           -> Script       -- ^ Plot Script
parcoord'' ns vs cfg = toScript layout traces
  where
    layout   = Just $ fromConfig cfg 
    traces   = [mkTraceP' ns vs]

-- | Save Plot to HTML File
plot :: FilePath -- ^ Target path to .html file
     -> Script   -- ^ Plotly Script
     -> IO ()
plot path = BL.writeFile path . toHtml
