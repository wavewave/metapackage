{-# LANGUAGE OverloadedStrings #-}

module Application.MetaPackage.Job where

import Application.MetaPackage.Config 
import Application.MetaPackage.Build
import Application.DevAdmin.Config 

import Control.Monad
import Data.Maybe 

startJob :: IO () 
startJob = do 
  putStrLn "job started"
  c1 <- loadConfigFile 
  mbc <- getBuildConfiguration c1
  mpc <- getProjectConfiguration c1 
  mmp <- getMetaProj =<< loadMetaProjConf "test.conf"

  maybe (return ()) id (liftM3 buildMetaPackage mbc mpc mmp )

  -- putStrLn $ show mp  

