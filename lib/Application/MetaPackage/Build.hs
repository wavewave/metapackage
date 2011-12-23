module Application.MetaPackage.Build where

import Distribution.PackageDescription
import Distribution.ModuleName

import Application.MetaPackage.Config
import Application.DevAdmin.Config 
import Application.DevAdmin.Cabal
import Application.DevAdmin.Project
import Control.Applicative
import Data.Foldable
import qualified Data.Map as M
import Data.Monoid
import Data.List (intercalate)
import Prelude hiding (foldr1,foldr, mapM_, concatMap)

buildMetaPackage :: BuildConfiguration -> ProjectConfiguration 
                 -> MetaProject -> IO ()
buildMetaPackage bc pc mp = do
  putStrLn "buildMetaPackage called"
  gdescs <- getAllGenPkgDesc bc pc 
  let epkgsdesc = getAllModules gdescs mp

  either (putStrLn ) ( mapM_ (putStrLn.show) .  map ((,) <$> modDirName <*> toFilePath ) ) epkgsdesc 
{-
  let namepkgpair = map ((,) <$> getPkgName <*> id) gdescs 
      namepkgmap = M.fromList namepkgpair
      pkgs = map projname . metaProjectPkg $ mp
      mmpkgsdesc = map (\k->M.lookup k namepkgmap) pkgs 
      mpkgsdesc = foldr (liftA2 (:)) (Just [])  mmpkgsdesc 
  flip (maybe (putStrLn "some packages are not present in project list")) 
       mpkgsdesc $ \pkgsdesc -> do 
         let pkgsmod = map getModules pkgsdesc 
         mapM_ (putStrLn . show) pkgsmod 
  --  mapM_  (putStrLn.show) pkgsmod 
  -- putStrLn $ show (map (\x->case x of Just _ -> "Just" ; _ -> "Nothing") pkgsdesc)
  

  return ()

-}

modDirName :: ModuleName -> FilePath 
modDirName = toFilePath . fromString . intercalate "." . init . components 

getAllModules :: [GenericPackageDescription] -> MetaProject 
              -> Either String [ModuleName] 
getAllModules gdescs mp = 
  let namepkgpair = map ((,) <$> getPkgName <*> id) gdescs 
      namepkgmap = M.fromList namepkgpair
      pkgs = map projname . metaProjectPkg $ mp
      mmpkgsdesc = map (\k->M.lookup k namepkgmap) pkgs 
      mpkgsdesc = foldr (liftA2 (:)) (Just [])  mmpkgsdesc 
  in maybe (Left "some packages are not present") 
           (Right . concatMap getModules)
           mpkgsdesc 


