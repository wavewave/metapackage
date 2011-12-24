module Application.MetaPackage.Command where

import Application.MetaPackage.ProgType
import Application.MetaPackage.Job

commandLineProcess :: Metapackage -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startTestJob
commandLineProcess Make = do 
  putStrLn "make called"
  startMakeJob