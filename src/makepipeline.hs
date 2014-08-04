module Main where

import System.Environment
import System.FilePath
--
import Driver
import MetaPackage

pipelineproj base = 
    MetaProject "metapipeline" [ AProject "webdav-manager"      (base </> "webdav-manager")
                               , AProject "pipeline-eventgen"   (base </> "pipeline-eventgen")
                               -- , AProject "pipeline"            (base </> "pipeline" </> "haskell-pipeline")
                               , AProject "madgraph-auto-model" (base </> "madgraph-auto-model")
                               , AProject "devadmin"            (base </> "devadmin")
                               , AProject "madgraph-auto"       (base </> "madgraph-auto")
                               , AProject "LHE-sanitizer"       (base </> "LHE-sanitizer")
                               , AProject "LHEParser"           (base </> "LHEParser")
                               , AProject "HEPUtil"             (base </> "HEPUtil")
                               , AProject "conduit-util"        (base </> "conduit-util")
                               , AProject "LHCOAnalysis-type"   (base </> "LHCOAnalysis-type")
                               , AProject "LHCOAnalysis"        (base </> "LHCOAnalysis")
                               , AProject "evchain"             (base </> "evchain")
                               , AProject "jobqueue-server"     (base </> "jobqueue-server" </> "oldcode" </> "jobqueue-server")
                               , AProject "jobqueue-client"     (base </> "jobqueue-server" </> "oldcode" </> "jobqueue-client")
                               , AProject "jobqueue-common"     (base </> "jobqueue-server" </> "oldcode" </> "jobqueue-common")
                               
                               , AProject "jobqueue-sender"     (base </> "jobqueue-sender")

                               -- , AProject "configparser"        (base </> "configparser")

                               ]   


main :: IO () 
main = do 
  args <- getArgs
  putStrLn "metapackage"
  
  makeMetaPackage (pipelineproj (args !! 0)) ""
  return ()
