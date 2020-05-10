module Main where

import qualified Example.Noise
import qualified Example.Annealing
import qualified Example.Clustering
import qualified Example.Cave2d
import qualified Example.CaveSprites
import qualified Example.Cave3d
import qualified Example.Mountains
import qualified Example.WFC
import qualified Example.Isometric

main :: IO ()
main = do

  -- Comparison of uniform, normal, and Perlin noise. There's not a huge
  -- difference between normal and uniform at this scale, but Perlin is quite
  -- strikingly different.
  putStrLn "Noise comparison..."
  Example.Noise.main

  -- Examples of "annealing" - smoothing noise by repeatedly averaging a kernel
  -- around a cell. The top line shows a 3x3 kernel being used to average the
  -- noise 5 times, while the bottom line shows a 5x5 kernel.
  putStrLn "Annealing stages..."
  Example.Annealing.main

  -- Examples of "clustering" - given some boolean noise, repeatedly assign a
  -- cell to the most common value in its kernel. The three rows show
  -- increasing "tolerance" - if the difference in numbers between the two
  -- boolean counts is within @2 * tolerance@, the cell is unchanged, meaning
  -- the output maintains its noise.
  putStrLn "Clustering stages..."
  Example.Clustering.main

  -- Putting together what we now know, we can draw a simple cave and render it
  -- in black and white.
  putStrLn "2D caves..."
  Example.Cave2d.main

  -- We can even use kernels to map the cells to sprites, to make things look a
  -- bit friendlier.
  putStrLn "2D caves with sprites..."
  Example.CaveSprites.main

  -- What's maybe not obvious before now, however, is that
  -- our process is polymorphic over any number of dimensions, so we can use
  -- the same result to produce __three-dimensional__ caves!
  putStrLn "3D caves..."
  Example.Cave3d.main

  -- Caves look best when they're this stochastic, but terrains often need to
  -- look a bit more gradual in their variations. For that reason, we can use
  -- Perlin noise to create something more appropriate. It takes a fair amount
  -- of (manual) tuning of the various parameters to the Perlin function to get
  -- the result you want, but it's great once you're there!
  putStrLn "Mountains..."
  Example.Mountains.main

  -- Another approach to procedural generation (that can be used alongside the
  -- previous techniques!) is WaveFunctionCollapse. The idea is that you
  -- specify, for each possible tile, what its neighbours could be. Then, using
  -- random selection and backtracking, the algorithm finds a configuration of
  -- tiles that satisfies all the connections between neighbours.
  putStrLn "WFC..."
  Example.WFC.main

  putStrLn "Iso..."
  Example.Isometric.main
