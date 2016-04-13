module HsInterface.Types where

import Data.List

data Export
    = SingleExport String
    | MultiExport String [String]
    deriving (Read, Show)


data Interface
    = Iface
    { absPath :: FilePath
    , pkgDirName :: FilePath
    , modNameParts :: [FilePath]
    , exports :: Maybe [Export]
    }
    deriving (Read, Show)

modName :: Interface -> String
modName Iface{modNameParts=parts} = intercalate "." parts
