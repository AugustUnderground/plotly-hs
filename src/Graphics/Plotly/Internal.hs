{-# OPTIONS_GHC -Wall -fno-warn-missing-export-lists #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Internal Functions and Types for Plotly Plots in Haskell
module Graphics.Plotly.Internal where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.ByteString.Lazy             (ByteString)

-- | Thanks Alan Zimmerman
-- https://gist.github.com/alanz/2465584
omitNulls :: [Pair] -> [Pair]
omitNulls = filter ((/= Null) . snd)

-- | Internal Type Alias
type Script = ByteString

-- | Takes a Script and inserts it into the default HTML template
toHtml :: Script -> ByteString
toHtml script = C8.unlines [ "<head>"
                           ,   "<script src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>"
                           , "</head>"
                           , "<body>"
                           ,   "<div id=\"plotDiv\"></div>"
                           ,   "<script>", script, "</script>"
                           , "</body>" ]

-- | Layout Margins
data Margin = Margin { l :: !Int -- ^ Left Margin
                     , r :: !Int -- ^ Right Margin
                     , b :: !Int -- ^ Bottom Margin
                     , t :: !Int -- ^ Top Margin
                     } deriving (Eq, Show, Generic, ToJSON)

-- | Layout for Plots
data Layout = Layout { title      :: !String          -- ^ Plot Title
                     , xaxis      :: !(Maybe Axis)    -- ^ X Axis Layout
                     , yaxis      :: !(Maybe Axis)    -- ^ Y Axis Layout
                     , barmode    :: !(Maybe BarMode) -- ^ Bar Chart Layout
                     , height     :: !Int             -- ^ Plot Height in Pixels
                     , width      :: !Int             -- ^ Plot Width in Pixels
                     , showlegend :: !(Maybe Bool)    -- ^ Show a Legend
                     , margin     :: !(Maybe Margin)  -- ^ Margins
                     } deriving (Show, Generic, ToJSON)

-- | Axis Type
data AxisType = Linear          -- ^ Linear Axis (default)
              | Log             -- ^ Logarithmic Axis
              | Date            -- ^ Date Axis
              | Category        -- ^ Categorical Axis
              | MultiCategory   -- ^ Multi Category Axis (?)
              deriving (Eq, Show, Generic, ToJSON)

-- | Axis Layout
data Axis = Axis { title    :: !String   -- ^ Axis Label
                 , showGrid :: !Bool     -- ^ Enable Grid
                 , zeroLine :: !Bool     -- ^ Enable Zero Line
                 , type'    :: !AxisType -- ^ Axis Type
                 } deriving (Show, Generic)

instance ToJSON Axis where
  toJSON Axis{..} = object [ "title"    .= title
                           , "showgrid" .= showGrid
                           , "zeroline" .= zeroLine
                           , "type"     .= type' ]

-- | Parallel Coordinate Lines
data Line = Line { showscale    :: !(Maybe Bool)       -- ^ Show Color Bar
                 , reversescale :: !(Maybe Bool)       -- ^ Reverse the Color Scale
                 , colorscale   :: !(Maybe ColorScale) -- ^ Color Scale
                 , color        :: ![Double]           -- ^ Values represnting the color
                 } deriving (Show, Generic, ToJSON)

-- | Marker Config
data Marker = Marker { size    :: !Double -- ^ Marker Size
                     , symbol  :: !Symbol -- ^ Marker Symbol
                     , opacity :: !Double -- ^ Marker Opacity
                     } deriving (Show, Generic, ToJSON)

-- | Data Trace
data Trace = Trace { name  :: !(Maybe String)   -- ^ Trace Name
                   , x     :: !(Maybe [Double]) -- ^ X axis values
                   , y     :: !(Maybe [Double]) -- ^ Y axis values
                   , z     :: !(Maybe [Double]) -- ^ Z axis values
                   , mode  :: !(Maybe Mode)     -- ^ Scatter Mode
                   , type' :: !Type             -- ^ Plot Type
                   , marker :: !(Maybe Marker)  -- ^ Marker Configuration
                   } deriving (Generic, Show)

instance ToJSON Trace where
  toJSON Trace{..} = object
                   $ omitNulls [ "name"   .= name
                               , "x"      .= x
                               , "y"      .= y
                               , "z"      .= z
                               , "mode"   .= mode
                               , "type"   .= type' ]

-- | Heatmap Trace
data TraceH = TraceH { z           :: ![[Double]]         -- ^ Data Matrix
                     , x           :: !(Maybe [String])   -- ^ Y labels
                     , y           :: !(Maybe [String])   -- ^ Y Labels
                     , type'       :: !Type               -- ^ Heatmap
                     , hoverOnGaps :: !(Maybe Bool)       -- ^ Hover on Gaps
                     , showScale   :: !(Maybe Bool)       -- ^ Show scale Bar
                     , colorScale  :: !(Maybe ColorScale) -- ^ Color Scale
                     } deriving (Show)

instance ToJSON TraceH where
  toJSON TraceH{..} = object [ "z"           .= z
                             , "x"           .= x
                             , "y"           .= y
                             , "hoverongaps" .= hoverOnGaps
                             , "showscale"   .= showScale
                             , "colorscale"  .= colorScale
                             , "type"        .= type' ]

-- | Surface Trace
data TraceS = TraceS { name        :: !(Maybe String)     -- ^ Trace Name
                     , z           :: ![[Double]]         -- ^ Data Matrix
                     , x           :: !(Maybe [Double])   -- ^ Y labels
                     , y           :: !(Maybe [Double])   -- ^ Y Labels
                     , type'       :: !Type               -- ^ Surface
                     , colorScale  :: !(Maybe ColorScale) -- ^ Color Scale
                     } deriving (Show)

instance ToJSON TraceS where
  toJSON TraceS{..} = object [ "name"       .= name
                             , "z"          .= z
                             , "x"          .= x
                             , "y"          .= y
                             , "colorscale" .= colorScale
                             , "type"       .= type' ]

-- | Parallel Coordinate Trace
data TraceP = TraceP { type'      :: !Type         -- ^ ParCoords
                     , line       :: !(Maybe Line) -- ^ Line Configuration
                     , dimensions :: ![Dimension]  -- ^ Dimensions
                     } deriving (Show)

instance ToJSON TraceP where
  toJSON TraceP{..} = object
                    $ omitNulls [ "dimensions" .= dimensions
                                , "line"       .= line
                                , "type"       .= type' ]

-- | Axis Dimension for Parallel Coordinate Plot
data Dimension = Dimension { range  :: !(Maybe [Int])
                           , label  :: !String
                           , values :: ![Double]
                           } deriving (Show, Generic, ToJSON)

-- | Plot Type
data Type = Scatter   -- ^ Scatter Plot
          | Histogram -- ^ Histogram
          | Heatmap   -- ^ Heatmap
          | Scatter3D -- ^ 3D Scatter
          | Surface   -- ^ 3D Surface
          | ParCoords -- ^ Parallel Coordinate Plot
          deriving (Eq, Generic, Show)

instance ToJSON Type where
  toJSON Scatter   = "scatter"
  toJSON Histogram = "histogram"
  toJSON Heatmap   = "heatmap"
  toJSON Scatter3D = "scatter3d"
  toJSON Surface   = "surface"
  toJSON ParCoords = "parcoords"

-- | Scatter Mode
data Mode = Lines        -- ^ Draw Lines
          | Markers      -- ^ Draw Markers only
          | LinesMarkers -- ^ Draw Both
          deriving (Eq, Generic, Show)

instance ToJSON Mode where
  toJSON Lines        = "lines"
  toJSON Markers      = "markers"
  toJSON LinesMarkers = "lines+markers"

-- | Marker Symbols
data Symbol = Circle
            | CircleOpen
            | Cross
            | Diamond
            | DiamondOpen
            | Square
            | SquareOpen
            | X
            deriving (Eq, Show, Generic)

instance ToJSON Symbol where
  toJSON Circle      = "circle"
  toJSON CircleOpen  = "circle-open"
  toJSON Cross       = "cross"
  toJSON Diamond     = "diamond"
  toJSON DiamondOpen = "diamond-open"
  toJSON Square      = "square"
  toJSON SquareOpen  = "square-open"
  toJSON X           = "x"

-- | Color scales for Heamap / 3D Plots
data ColorScale = Blackbody
                | Bluered
                | Blues
                | Cividis
                | Earth
                | Electric
                | Greens
                | Greys
                | Hot
                | Jet
                | Picnic
                | Portland
                | Rainbow
                | RdBu
                | Reds
                | Viridis
                | YlGnBu
                | YlOrRd
  deriving (Eq, Show, Generic, ToJSON)

-- | Bar Layout
data BarMode = Stack    -- ^ Stacked
             | Group    -- ^ Grouped
             | Overlay  -- ^ Overlayed
             | Relative -- ^ Relative
             deriving (Eq, Generic, Show)

instance ToJSON BarMode where
  toJSON Stack    = "stack"
  toJSON Group    = "group"
  toJSON Overlay  = "overlay"
  toJSON Relative = "relative"

-- | Generates line for @data@ variable for @N@ traces
traceData :: Int -> Script
traceData num = BL.concat [ "var data = [", ids, "];" ]
  where
    ids  = C8.intercalate "," [ C8.pack $ "trace" ++ show i | i <- [0 .. num - 1] ]

-- | Generates line for @i@th @trace@ variable
traceLine :: (ToJSON a) => a -> Int -> ByteString
traceLine t i = BL.concat ["var trace", i', " = ", encode t, ";"]
  where
    i' = C8.pack (show i)

-- | Converts layout and list of traces to @Script@ for insertion into HTML
toScript :: (ToJSON a) => Maybe Layout -> [a] -> ByteString
toScript layout traces = C8.unlines $ ls ++ [ds, lay, "Plotly.newPlot('plotDiv', data, layout);"]
  where
    num = length traces
    ls  = zipWith traceLine traces [ 0 .. ]
    ds  = traceData num
    lay = BL.concat ["var layout = ", encode layout, ";"]

-- | Trace contructor, different order of arguments
mkTrace :: Maybe String -> Maybe Mode -> Maybe Marker -> Type
        -> [Double] -> [Double] -> [Double] -> Trace
mkTrace n m m' t xs ys zs = Trace n xs' ys' zs' m t m'
  where
    xs' = if null xs then Nothing else Just xs
    ys' = if null ys then Nothing else Just ys
    zs' = if null zs then Nothing else Just zs

-- | Heatmap Trace constructor
mkTraceH :: ColorScale -> Bool -> Bool -> [String] -> [String] -> [[Double]] -> TraceH
mkTraceH cs hv sc xs ys zs = TraceH zs xs' ys' Heatmap (Just hv) (Just sc) (Just cs)
  where
    ys' = if null ys then Nothing else Just ys
    xs' = if null xs then Nothing else Just xs

-- | Heatmap Trace constructor
mkTraceS :: Maybe String -> ColorScale -> [Double] -> [Double] -> [[Double]] -> TraceS
mkTraceS ns cs xs ys zs = TraceS ns zs xs' ys' Surface (Just cs)
  where
    ys' = if null ys then Nothing else Just ys
    xs' = if null xs then Nothing else Just xs

-- | Parallel Coordinate Trace Constructor
mkTraceP :: ColorScale -> Bool -> Bool -> [Double] -> [String] -> [[Double]] -> TraceP
mkTraceP scale show' rev' colors labels ds = TraceP ParCoords (Just line) dims
  where
    line = Line (Just show') (Just rev') (Just scale) colors
    dims = zipWith (Dimension Nothing) labels ds

-- | Parallel Coordinate Trace Constructor without Line configuration
mkTraceP' :: [String] -> [[Double]] -> TraceP
mkTraceP' labels ds = TraceP ParCoords Nothing dims
  where
    dims = zipWith (Dimension Nothing) labels ds
