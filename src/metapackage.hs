module Main where

-- import System.Console.CmdArgs

-- import Application.MetaPackage.ProgType
-- import Application.MetaPackage.Command

import Driver
import MetaPackage

testmetaproj = MetaProject "metahoodle" [ AProject "hoodle-render" "/home/wavewave/repo/src/hoodle/hoodle-render"
                                        , AProject "hoodle-core" "/home/wavewave/repo/src/hoodle/hoodle-core" 
                                        , AProject "hoodle-types" "/home/wavewave/repo/src/hoodle/hoodle-types"
                                        ]   

main :: IO () 
main = do 
  putStrLn "metapackage"
  makeMetaPackage testmetaproj
  -- (pkgpath,srcpath) <- initializeMetaPackage testmetaproj
  -- print (pkgpath,srcpath)
