{-# LANGUAGE OverloadedStrings #-}

module Application.MetaPackage.Config where

import Application.DevAdmin.Project
import Control.Applicative
import Data.Configurator.Types 
import Data.Configurator as C

data MetaProject = MetaProject { metaProjectName :: String
                               , metaProjectPkg :: [Project]  
                               }
                 deriving (Show)

loadMetaProjConf :: FilePath -> IO Config 
loadMetaProjConf fp = load [Required fp] 

getMetaProj :: Config -> IO (Maybe MetaProject) 
getMetaProj c = 
  liftA2 MetaProject <$> C.lookup c "MetaProjectName" 
                     <*> C.lookup c "MetaProjectPkg"
