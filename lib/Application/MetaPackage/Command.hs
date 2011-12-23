module Application.MetaPackage.Command where

import Application.MetaPackage.ProgType
import Application.MetaPackage.Job

commandLineProcess :: Metapackage -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
