module Main where

import GHC.Paths
import Data.List

import Control.Monad
import qualified Control.Monad.Parallel as Parallel

import System.Environment

import HsInterface
 
findMode :: IO ()
findMode = findIfaces libdir >>= Parallel.mapM resolveIface >>= print

getMode :: FilePath -> IO ()
getMode p = do
    info <- read <$> readFile p :: IO [Interface]
    print info        

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> findMode
        [p] -> getMode p
        _ -> putStrLn $ "0 args to find, 1 arg to read"

