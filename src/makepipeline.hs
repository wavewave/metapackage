module Main where

import System.Environment
import System.FilePath
--
import Driver
import MetaPackage

testmetaproj base = 
    MetaProject "metahoodle" [ AProject "hoodle-types"   (base </> "hoodle-types")
                             , AProject "xournal-types"  (base </> "xournal-types")
                             , AProject "xournal-parser" (base </> "xournal-parser")
                             , AProject "hoodle-parser"  (base </> "hoodle-parser")
                             , AProject "hoodle-builder" (base </> "hoodle-builder")
                             , AProject "hoodle-render"  (base </> "hoodle-render")
                             , AProject "hoodle-core"    (base </> "hoodle-core")
                             ]   


main :: IO () 
main = do 
  args <- getArgs
  putStrLn "metapackage"
  
  makeMetaPackage (testmetaproj "/home/wavewave/repo/src/hoodle") ""
  return ()
  -- (pkgpath,srcpath) <- initializeMetaPackage testmetaproj
  -- print (pkgpath,srcpath)
