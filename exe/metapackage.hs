module Main where

import System.Console.CmdArgs

import Application.MetaPackage.Type
import Application.MetaPackage.Command

main :: IO () 
main = do 
  putStrLn "metapackage"
  param <- cmdArgs mode

  commandLineProcess param