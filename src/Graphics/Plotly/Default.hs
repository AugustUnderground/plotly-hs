{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE DuplicateRecordFields #-}

-- | Default Plot Configuration
module Graphics.Plotly.Default ( PlotConfig (..), defaultConfig, fromConfig
                               ) where

import Graphics.Plotly.Internal

-- | Config to Layout
fromConfig :: PlotConfig -> Layout
fromConfig PlotConfig{..} = layout
  where
    xaxis  = Just $ Axis xlabel True True xmode
    yaxis  = Just $ Axis ylabel True True ymode
    barm   = Just barMode
    layout = Layout title' xaxis yaxis barm height width
                    (Just legend) (Just margin)

-- | Trace Independent Plot Config
data PlotConfig = PlotConfig { title'       :: !String     -- ^ Plot Title (default "")
                             , xlabel       :: !String     -- ^ X Axis Label (default "")
                             , ylabel       :: !String     -- ^ Y Axis Label (default "")
                             , xmode        :: !AxisType   -- ^ X Axis Type (default Linear)
                             , ymode        :: !AxisType   -- ^ Y Axis Type (default Linear)
                             , barMode      :: !BarMode    -- ^ Bar Mode (default Stack)
                             , lineMode     :: !Mode       -- ^ Line Mode (default Linear)
                             , colorScale   :: !ColorScale -- ^ Color Scale for 3D (default Viridis)
                             , showScale    :: !Bool       -- ^ Show Color Bar (default True)
                             , reverseScale :: !Bool       -- ^ Reverse the color scale (default False)
                             , legend       :: !Bool       -- ^ Show Legend (default True)
                             , height       :: !Int        -- ^ Canvas Height in Pixels (default 800)
                             , width        :: !Int        -- ^ Canvas Width in Pixels (default 800)
                             , margin       :: !Margin     -- ^ Layout margin (default 66 66 66 66)
                             , marker       :: !Marker     -- ^ Marker (default 0.5 Circle 1.0)
                             , bins         :: !XBins      -- ^ Histogram bins (default 0.05 Nothing Nothing)
                             } deriving (Show)

-- | Default @PlotConfig@
defaultConfig :: PlotConfig
defaultConfig = PlotConfig { title'       = ""
                           , xlabel       = ""
                           , ylabel       = ""
                           , xmode        = Linear
                           , ymode        = Linear
                           , barMode      = Overlay
                           , lineMode     = Lines
                           , colorScale   = Viridis
                           , showScale    = True
                           , reverseScale = False
                           , legend       = True
                           , height       = 800
                           , width        = 800
                           , margin       = Margin 66 66 66 66
                           , marker       = Marker 0.1 Circle 1.0
                           , bins         = XBins 0.05 Nothing Nothing
                           }
