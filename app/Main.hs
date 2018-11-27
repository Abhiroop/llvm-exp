module Main where

import Lib

import Control.Monad.Par
import Data.Vector

parMap :: NFData b => (a -> b) -> Vector a -> Vector b
parMap f as = runPar $ do
    ibs <- Data.Vector.mapM (spawn . return . f) as
    Data.Vector.mapM get ibs

main :: IO ()
main = someFunc
