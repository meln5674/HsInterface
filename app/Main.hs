module Main where

import GHC.Paths
import Data.List

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import System.Process
import System.FilePath

import Control.Monad
import qualified Control.Monad.Parallel as Parallel

import System.Environment

import HsInterface

getLocalLibDir = do
    path <- readProcess "stack" ["path", "--local-install-root"] ""
    return $ init path
 
getSnapshotLibDir = do
    path <- readProcess "stack" ["path", "--snapshot-install-root"] ""
    return $ init path


findMode :: FilePath -> IO ()
findMode archstr = do
    libdirIfaces <- findIfaces libdir
    snapshotDir <- getSnapshotLibDir
    localDir <- getLocalLibDir
    snapshotIfaces <- findIfaces (snapshotDir </> "lib" </> archstr)
    localIfaces <- findIfaces (localDir </> "lib" </> archstr)
    let allIfaces = libdirIfaces ++ snapshotIfaces ++ localIfaces
    let nubbedIfaces = Map.elems $ Map.fromList $ map (\i -> (modNameParts i,i)) allIfaces
    --let allIfaces = snapshotIfaces
    Parallel.mapM resolveIface nubbedIfaces >>= print

getMode :: FilePath -> IO ()
getMode p = do
    info <- read <$> readFile p :: IO [Interface]
    print info        

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-f", archstr] -> findMode archstr
        [p] -> getMode p
        _ -> mapM_ putStrLn $ 
            [ "USAGE"
            , "HsInterface PATH - get interface info at PATH"
            , "HsInterface -f ARCHSTR - list all interfaces for ARCHSTR"
            , "\ti.e. x86_64-linux-ghc-8.0.1"
            ]

