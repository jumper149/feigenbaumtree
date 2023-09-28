{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Feigenbaumtree where

import Data.Foldable
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Options.Applicative qualified as Opt

data Config = MkConfig
    { configXDepth :: Word
    , configYDepth :: Word
    , configCalcDepth :: Word
    , configSize :: Word
    }

options :: Opt.ParserInfo Config
options = Opt.info (Opt.helper <*> parser) docs
  where
      parser = do
          configXDepth <- Opt.option Opt.auto $ Opt.long "x-depth"
          configYDepth <- Opt.option Opt.auto $ Opt.long "y-depth"
          configCalcDepth <- Opt.option Opt.auto $ Opt.long "calc-depth"
          configSize <- Opt.option Opt.auto $ Opt.long "size"
          pure MkConfig {..}
      docs = Opt.fullDesc @Config

main :: IO ()
main = do
  config :: Config <- Opt.execParser options
  let result = program config
  T.writeFile "haskell.magick" $ magickScript (configSize config) result
  T.putStrLn "Generated script for imagemagick."

program :: Config -> [(Double, Double)]
program MkConfig{..} = V.toList dots
  where
    rs :: V.Vector Double
    rs = range configXDepth 0 4

    inits :: Double -> V.Vector Double
    inits r = range configYDepth 0 (r / 4)

    rAndInits :: V.Vector (Double, V.Vector Double)
    rAndInits = f <$> rs
     where
      f r = (r, inits r)

    dots :: V.Vector (Double, Double)
    dots = fold coords
     where
      applyLogisticMap :: (Double, V.Vector Double) -> (Double, V.Vector Double)
      applyLogisticMap (r, xs) = (r, times configCalcDepth (logisticMapDouble r) <$> xs)
      rAndFinals :: V.Vector (Double, V.Vector Double)
      rAndFinals = applyLogisticMap <$> rAndInits
      coords :: V.Vector (V.Vector (Double, Double))
      coords = (\(r, finals) -> (r,) <$> finals) <$> rAndFinals

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

drawStatement :: Word -> (Double, Double) -> T.Text
drawStatement maxSize (x, y) = "-draw \"point " <> T.pack (show newX) <> "," <> T.pack (show newY) <> "\""
 where
  newX = x * (fromIntegral maxSize / 4)
  newY = fromIntegral maxSize - y * (fromIntegral maxSize / 1)

magickScript :: Word -> [(Double, Double)] -> T.Text
magickScript maxSize coordinates =
  T.unlines $
    [ "#!/usr/bin/env magick-script"
    , T.pack $ "-size " <> show maxSize <> "x" <> show maxSize <> " canvas:none"
    , "-fill rgba(0,0,0,0.3)"
    ]
      ++ (drawStatement maxSize <$> coordinates)
      ++ [ "-blur 1x2"
         , "-write haskell.png"
         ]
