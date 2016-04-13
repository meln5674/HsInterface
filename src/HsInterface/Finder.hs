module HsInterface.Finder where

import Control.Monad
import Data.List

import System.FilePath
import System.Directory
import System.Process

import HsInterface.Types

getChildren :: FilePath -> IO ([FilePath],[FilePath])
getChildren p = do
    children' <- getDirectoryContents p
    let children = filter (\x -> x /= ".." && x /= ".") children'
    files <- filterM (doesFileExist . (p </>)) children
    dirs <- filterM (doesDirectoryExist . (p </>)) children
    return (files, dirs)

findIfacesIn :: FilePath -> FilePath -> [FilePath] -> IO [Interface]
findIfacesIn top pkgDir pkgDirParts = do
    let pkgDirAbs = top </> (foldl (</>) pkgDir pkgDirParts)
    --putStrLn $ "Looking in : " ++ pkgDirAbs
    (pkgDirFiles,pkgDirDirs) <- getChildren pkgDirAbs
    let pkgDirIfaceFiles = filter (".hi" `isSuffixOf`) pkgDirFiles
    let mkIface p = Iface (pkgDirAbs </> p) pkgDir (pkgDirParts ++ [take (length p - length ".hi") p]) Nothing
    let ifaces = map mkIface pkgDirIfaceFiles
    childIfaces <- mapM (findIfacesIn top pkgDir . (pkgDirParts ++) . (:[])) pkgDirDirs
    return $ ifaces ++ concat childIfaces

findIfaces :: FilePath -> IO [Interface]
findIfaces top = do
    (_,pkgDirs) <- getChildren top
    ifaces <- mapM (\d -> findIfacesIn top d []) pkgDirs
    return $ concat ifaces

printExport :: Export -> IO ()
printExport (SingleExport s) = putStrLn s
printExport (MultiExport a bs) = putStrLn $ a ++ "{" ++ unwords bs ++ "}"

printIface :: Interface -> IO ()
printIface i = do
    putStrLn $ modName i ++ " in " ++ pkgDirName i ++ " (" ++ absPath i ++ " )"
    case exports i of
        Nothing -> putStrLn "   No exports yet"
        Just es -> mapM_ printExport es

parseShowIfaceOutput :: String -> [String]
parseShowIfaceOutput = findLastExport . findFirstExport . lines
  where
    findFirstExport = drop 1 . dropWhile (/= "exports:")
    findLastExport = takeWhile ("  " `isPrefixOf`)

findInBrackets :: String -> Maybe (String,String)
findInBrackets s = case findLastBracket $ findFirstBracket s of
    [] -> Nothing
    s' -> Just (beforeBrackets s, s')
  where
    beforeBrackets = takeWhile (/= '{')
    findFirstBracket = drop 1 . dropWhile (/= '{')
    findLastBracket = takeWhile (/= '}')

parseExport :: String -> Export
parseExport (' ':' ':s) = case findInBrackets s of
    Nothing -> SingleExport s
    Just (pre,post) -> MultiExport pre (words post)

showIfaceCommand :: String -> IO String
showIfaceCommand p = readProcess "stack" ["ghc", "--", "--show-iface", p] ""

resolveIface :: Interface -> IO Interface
resolveIface i = do
    let path = absPath i
    cmdOutput <- showIfaceCommand path
    let exports' = map parseExport $ parseShowIfaceOutput cmdOutput
    return $ i{exports=Just exports'}
