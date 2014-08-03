module Paths_$pkgname$ where

import Data.Version

getDataDir :: IO FilePath
getDataDir = return "$datapath$"

version :: Version
version = Version [0,0] []


