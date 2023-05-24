{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Control.Monad
import System.Random
import Graphics.Plotly

plotPath :: String -> FilePath
plotPath name = "./plots/" ++ name ++ ".html"

main :: IO ()
main = do

    let scatterPlot = scatter' ["foo", "bar"]
                               [[1, 10, 100, 1000], [1, 10, 100, 1000]]
                               [[10, 15, 13, 17],[16, 5, 11, 9]]
                    $ defaultConfig { xmode = Log }
    plot (plotPath "scatter") scatterPlot

    x1 <- replicateM 100 (randomRIO (0, 5) ) :: IO [Double]
    x2 <- replicateM 100 (randomRIO (0, 5) ) :: IO [Double]
    x3 <- replicateM 100 (randomRIO (0, 5) ) :: IO [Double]
    let histPlot = hist ["x1", "x2", "x3"] [x1,x2,x3]
                 $ defaultConfig {title' = "Plot Title"}
    plot (plotPath "hist") histPlot

    let heatPlot = heatmap ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
                           ["Morning", "Afternoon", "Evening"]
                           [[1, 0, 30, 50, 1], [20, 1, 60, 80, 30], [30, 60, 1, -10, 20]]
                           defaultConfig
    plot (plotPath "heatmap") heatPlot

    let scatter3dPlot = scatter3d ["point cloud"] [[1, 0, 30, 50, 1]] 
                                  [[20, 1, 60, 80, 30]] [[30, 60, 1, -10, 20]]
                      $ defaultConfig {lineMode = Markers}
    plot (plotPath "scatter3d") scatter3dPlot

    let xx = map ((*10) . subtract 5) [0 .. 14]
        yy = [2,4,6,8,10,12]
        zz = [ [8.83,8.89,8.81,8.87,8.9,8.87]
             , [8.89,8.94,8.85,8.94,8.96,8.92]
             , [8.84,8.9,8.82,8.92,8.93,8.91]
             , [8.79,8.85,8.79,8.9,8.94,8.92]
             , [8.79,8.88,8.81,8.9,8.95,8.92]
             , [8.8,8.82,8.78,8.91,8.94,8.92]
             , [8.75,8.78,8.77,8.91,8.95,8.92]
             , [8.8,8.8,8.77,8.91,8.95,8.94]
             , [8.74,8.81,8.76,8.93,8.98,8.99]
             , [8.89,8.99,8.92,9.1,9.13,9.11]
             , [8.97,8.97,8.91,9.09,9.11,9.11]
             , [9.04,9.08,9.05,9.25,9.28,9.27]
             , [9,9.01,9,9.2,9.23,9.2]
             , [8.99,8.99,8.98,9.18,9.2,9.19]
             , [8.93,8.97,8.97,9.18,9.2,9.18] ]
        surfPlot'' = surface'' ["surface"] [zz] defaultConfig
        surfPlot'  = surface'  ["surface"] xx yy [zz] defaultConfig
    plot (plotPath "surface1") surfPlot''
    plot (plotPath "surface2") surfPlot'

    let as = [1,2,3,5]
        bs = [3.1,1.5,4.5,9.3]
        cs = [2,4,6,8]
        ds = [-3,-5,-10,-7]
        c' = [0.2,0.4,0.6,0.8]
        parPlot' = parcoord' ["A", "B", "C", "D"] [as,bs,cs,ds]
                 $ defaultConfig {width = 1500, height = 600}
        parPlot = parcoord c' ["A", "B", "C", "D"] [as,bs,cs,ds]
                $ defaultConfig {width = 1500, height = 600}
    plot (plotPath "parcoords1") parPlot'
    plot (plotPath "parcoords2") parPlot

    pure ()
