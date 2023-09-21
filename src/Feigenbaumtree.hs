module Feigenbaumtree where

import Data.Foldable
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V

main :: IO ()
main = do
  T.writeFile "haskell.magick" magickScript

logisticMap :: Num a => a -> a -> a
logisticMap r xn = r * xn * (1 - xn)

logisticMapDouble :: Double -> Double -> Double
logisticMapDouble = logisticMap

times :: Word -> (a -> a) -> a -> a
times 0 f x = f x
times n f x = times (pred n) f (f x)

range :: Word -> Double -> Double -> V.Vector Double
range n x y = V.fromList $ times n f [x, y]
 where
  f :: [Double] -> [Double]
  f [] = []
  f [a] = [a]
  f (a : b : cs) = a : ((a + b) / 2) : f (b : cs)

rs :: V.Vector Double
rs = range 8 0 4

inits :: Double -> V.Vector Double
inits r = range 6 0 (r / 4)

rAndInits :: V.Vector (Double, V.Vector Double)
rAndInits = f <$> rs
 where
  f r = (r, inits r)

dots :: V.Vector (Double, Double)
dots = fold coords
 where
  applyLogisticMap :: (Double, V.Vector Double) -> (Double, V.Vector Double)
  applyLogisticMap (r, xs) = (r, times 5 (logisticMapDouble r) <$> xs)
  rAndFinals :: V.Vector (Double, V.Vector Double)
  rAndFinals = applyLogisticMap <$> rAndInits
  coords :: V.Vector (V.Vector (Double, Double))
  coords = (\(r, finals) -> (r,) <$> finals) <$> rAndFinals

drawStatement :: (Double, Double) -> T.Text
drawStatement (x, y) = "-draw \"point " <> T.pack (show newX) <> "," <> T.pack (show newY) <> "\""
 where
  newX = x * (3000 / 4)
  newY = 3000 - y * (3000 / 1)

magickScript :: T.Text
magickScript =
  T.unlines $
    [ "#!/usr/bin/env magick-script"
    , "-size 3000x3000 canvas:none"
    ]
      ++ (drawStatement <$> V.toList dots)
      ++ ["-write haskell.png"]
