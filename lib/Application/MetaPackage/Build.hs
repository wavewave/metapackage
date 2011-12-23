module Application.MetaPackage.Build where

import Distribution.PackageDescription
import Distribution.ModuleName

import Application.MetaPackage.Config
import Application.DevAdmin.Config 
import Application.DevAdmin.Cabal
import Application.DevAdmin.Project
import Control.Applicative
import Control.Monad hiding (mapM_)
import Data.Foldable
import qualified Data.Map as M
import Data.Monoid
import Data.List (intercalate)


import System.Directory
import System.FilePath
import System.Process

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Paths_metapackage
import Prelude hiding (foldr1,foldr, mapM_, concatMap, concat)

-- | starting job

buildMetaPackage :: BuildConfiguration -> ProjectConfiguration 
                 -> MetaProject -> IO ()
buildMetaPackage bc pc mp = do
  putStrLn "buildMetaPackage called"
  gdescs <- getAllGenPkgDesc bc pc 
  let epkgsdesc = getAllModules gdescs mp

  either putStrLn (makeMetaPackage bc mp) epkgsdesc 

-- | driver IO action for make a meta package

makeMetaPackage :: BuildConfiguration -> MetaProject 
                -> [(Project,[(FilePath,ModuleName)])] 
                -> IO ()
makeMetaPackage  bc mp allmodules = do 
  (pkgpath,srcpath) <- initializeMetaPackage mp
  let allmodnames = do 
        (p,ns) <- allmodules
        (fp,m) <- ns  
        return m
  makeCabalFile pkgpath mp allmodnames  
  mapM_ (linkMod srcpath) . concatMap (absolutePathModuleAll bc <$> fst <*> snd) $ allmodules 

-- | create a metapackage cabal file name. 

makeCabalFile :: FilePath -> MetaProject 
              -> [ModuleName]
              -> IO ()
makeCabalFile pkgpath mp modnames = do 
  tmpldir <- getDataDir >>= return . (</> "template")
  tmpl <- directoryGroup tmpldir 
  let exposedmodules = concatMap ("\n          "++) 
                       . map (intercalate "." . components) 
                       $ modnames
  let replacement = [ ("projname",metaProjectName mp)
                    , ("licensetype", ""  ) 
                    , ("executable", "" )
                    , ("libdep", "" ) 
                    , ("exedep", "")
                    , ("exposedmodules", exposedmodules)
                    , ("modulebase", "src")
                    , ("progtype", "") 
                    ] 
  let cabalstr = renderTemplateGroup tmpl replacement "project.cabal"
  writeFile (pkgpath </> metaProjectName mp <.> "cabal") cabalstr 
                  

-- | create metapackage directory and return such created directory names

initializeMetaPackage :: MetaProject -> IO (FilePath,FilePath)
initializeMetaPackage mp = do 
  cdir <- getCurrentDirectory 
  let pkgpath = cdir </> (metaProjectName mp)
      srcpath = cdir </> (metaProjectName mp </> "src")
  createDirectory pkgpath
  createDirectory srcpath 
  return (pkgpath,srcpath)


-- | for all modules 

absolutePathModuleAll :: BuildConfiguration -> Project 
                         -> [(FilePath,ModuleName)] -> [(FilePath,ModuleName)]
absolutePathModuleAll bc proj xs = 
  map (absolutePathModule bc proj) xs 


-- | relative path info to absolute path info for modules 

absolutePathModule :: BuildConfiguration -> Project 
                   -> (FilePath,ModuleName) -> (FilePath,ModuleName)
absolutePathModule bc proj (fp,modname) = 
  let absolutify dir = bc_progbase bc </> projname proj </> dir
  in (absolutify fp,modname)

-- | create module directory and link the original file in the destination.

linkMod :: FilePath -> (FilePath,ModuleName) -> IO () 
linkMod srcdir (fp,modname) = do 
  createModuleDirectory srcdir modname
  checkAndLinkModuleFile srcdir (fp,modname) 

-- | check whether original source file exists and link it in the destination.

checkAndLinkModuleFile :: FilePath -> (FilePath,ModuleName) -> IO () 
checkAndLinkModuleFile srcdir (fp,modname) = do 
  let origfilename = fp </> (toFilePath modname)
  doesFileExist (origfilename <.> "hs") >>= \x -> when x $ do 
    system $ "ln -s " ++ (origfilename <.> "hs") ++ " " ++ (srcdir </>  toFilePath modname <.> "hs")
    return ()


-- | create module directory if not exist 

createModuleDirectory :: FilePath -> ModuleName -> IO () 
createModuleDirectory basedir modname = do 
  let newdirname = basedir </> modDirName modname 
  doesDirectoryExist newdirname >>= \b -> 
    when (not b) (do system ("mkdir -p " ++ newdirname) ; return ())
  return ()


-- | get module directory name (omitting last part of module) 

modDirName :: ModuleName -> FilePath 
modDirName = toFilePath . fromString . intercalate "." . init . components 


-- | get a library module information for a single package

getModulesForOnePkg :: M.Map String GenericPackageDescription -> Project 
                    -> Either String [(FilePath,ModuleName)]
getModulesForOnePkg namepkgmap proj = 
  let mpkgdesc = M.lookup (projname proj) namepkgmap 
  in maybe (Left ("package " ++ projname proj ++ " doesn't exist"))
           (Right . getModules) 
           mpkgdesc      

-- | get a library module information for all packages in the metapackage

getAllModules :: [GenericPackageDescription] -> MetaProject 
              -> Either String [(Project,[(FilePath,ModuleName)])] 
getAllModules gdescs mp = 
  let namepkgpair = map ((,) <$> getPkgName <*> id) gdescs 
      namepkgmap = M.fromList namepkgpair
      pkgs = metaProjectPkg mp
      makeresult  pkg = (,) pkg <$> getModulesForOnePkg namepkgmap pkg
  in mapM makeresult pkgs 


