{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Graphics.Plotly.Example

main :: IO ()
main = do
    scatterExample
    histExample
    heatmapExample
    scatter3dExample
    surfaceExample
    parcoordsExample
