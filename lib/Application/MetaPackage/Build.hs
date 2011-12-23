module Application.MetaPackage.Build where

import Debug.Trace

import Distribution.Package
import Distribution.PackageDescription
import Distribution.ModuleName
import Distribution.Version

import Application.MetaPackage.Config
import Application.DevAdmin.Config 
import Application.DevAdmin.Cabal
import Application.DevAdmin.Project
import Control.Applicative
import Control.Monad hiding (mapM_,msum)
import Data.Foldable
import Data.Function
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.List (intercalate,group,sort,sortBy)


import System.Directory
import System.FilePath
import System.Process

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Paths_metapackage
import Prelude hiding (foldr1,foldr, mapM_, concatMap, concat, sum, elem)

-- | starting job

buildMetaPackage :: BuildConfiguration -> ProjectConfiguration 
                 -> MetaProject -> IO ()
buildMetaPackage bc pc mp = do
  putStrLn "buildMetaPackage called"
  gdescs <- getAllGenPkgDesc bc pc 
  let epkgsdesc = getAllProjPkg gdescs mp 


-- getAllModules gdescs mp

  either putStrLn (makeMetaPackage bc mp) epkgsdesc 

-- | driver IO action for make a meta package

makeMetaPackage :: BuildConfiguration -> MetaProject 
                -> [ProjectPkg]
                -> IO ()
makeMetaPackage  bc mp pkgs = do
  let allmodules = getAllModules pkgs  
  (pkgpath,srcpath) <- initializeMetaPackage mp
  let allmodnames = do 
        (p,ns) <- allmodules
        (fp,m) <- ns  
        return m
  makeCabalFile pkgpath mp pkgs allmodnames  
  mapM_ (linkMod srcpath) . concatMap (absolutePathModuleAll bc <$> fst <*> snd) $ allmodules 
  

depString :: MetaProject -> [ProjectPkg] -> String 
depString mp pkgs =  
  let depsall = intersectionDeps mp . concatMap getDepsForOnePkg $ pkgs
      formatter (Dependency (PackageName pname) vr) = 
        pname ++ versionRangeString vr 
  in unlines . map ((++ ",") . ("         " ++) . formatter) $ depsall




-- | create a metapackage cabal file name. 

makeCabalFile :: FilePath -> MetaProject -> [ProjectPkg]
              -> [ModuleName]
              -> IO ()
makeCabalFile pkgpath mp pkgs modnames = do 
  tmpldir <- getDataDir >>= return . (</> "template")
  tmpl <- directoryGroup tmpldir 
  let exposedmodules = concatMap ("\n          "++) 
                       . map (intercalate "." . components) 
                       $ modnames
      libdep = depString mp pkgs 
  let replacement = [ ("projname",metaProjectName mp)
                    , ("licensetype", ""  ) 
                    , ("executable", "" )
                    , ("libdep", libdep ) 
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


type ProjectPkg = (Project,GenericPackageDescription)

-- | get Project and Generic Package Description

getProjPkg :: M.Map String GenericPackageDescription -> Project  
           -> Either String ProjectPkg
getProjPkg namepkgmap proj = 
  let mpkgdesc = M.lookup (projname proj) namepkgmap 
  in maybe (Left ("package " ++ projname proj ++ " doesn't exist"))
           (Right . (,) proj ) 
           mpkgdesc      
  
-- | get a library pkg information for all packages in the metapackage

getAllProjPkg :: [GenericPackageDescription] -> MetaProject 
              -> Either String [ProjectPkg]
getAllProjPkg gdescs mp = 
  let namepkgpair = map ((,) <$> getPkgName <*> id) gdescs 
      namepkgmap = M.fromList namepkgpair
      pkgs = metaProjectPkg mp
  in mapM (getProjPkg namepkgmap) pkgs 


-- | get a library module information for a single package

getModulesForOnePkg :: ProjectPkg -> [(FilePath,ModuleName)]
getModulesForOnePkg (proj,desc) = getModules desc 
  

-- | get all library module information

getAllModules :: [ProjectPkg] -> [(Project,[(FilePath,ModuleName)])] 
getAllModules pkgs = map ((,) <$> fst <*> getModulesForOnePkg) pkgs 


-- | get a library dep information 


getDepsForOnePkg :: ProjectPkg -> [Dependency]
getDepsForOnePkg (proj,desc) = 
  let rlib = condLibrary desc
  in maybe [] (condTreeConstraints) rlib


newtype DependencyEqName = DepEqName { unDepEqName :: Dependency } 
    deriving (Show)

instance Eq DependencyEqName where
  (DepEqName (Dependency n1 _))  == (DepEqName (Dependency n2 _))
    = n1 == n2


dep_pkgname :: DependencyEqName -> PackageName
dep_pkgname (DepEqName (Dependency n1 _)) = n1 

dep_vrange :: DependencyEqName -> VersionRange 
dep_vrange (DepEqName (Dependency _ v)) = v 

instance Monoid VersionRange where
  mappend = intersectVersionRanges 
  mempty = anyVersion


-- | get intersection dep

intersectionDeps :: MetaProject -> [Dependency] -> [Dependency] 
intersectionDeps mp deps = 
  let grouped = group . sortBy (compare `on` dep_pkgname) . map DepEqName $ deps
      projnames = map projname . metaProjectPkg $ mp
      filterMP x = case (dep_pkgname . head) x of 
                     PackageName pname -> if elem pname projnames
                                            then Nothing
                                            else Just (PackageName pname,x)
      filtered = mapMaybe filterMP grouped
 
  in map (Dependency <$> fst <*> simplifyVersionRange . mconcat . map dep_vrange . snd) filtered
  
-- | dependency string 
{-
dependencyString :: [Dependency] -> String 
dependencyString =  -}

versionString :: Version -> String 
versionString (Version b _ ) = intercalate "." (map show b)

versionRangeString :: VersionRange -> String
versionRangeString AnyVersion = "" 
versionRangeString (ThisVersion v) = "==" ++ versionString v
versionRangeString (LaterVersion v) = ">" ++ versionString v
versionRangeString (EarlierVersion v) = "<" ++ versionString v
versionRangeString (UnionVersionRanges vr1 vr2) =
  "(" ++ versionRangeString vr1 ++ ") || (" ++ versionRangeString vr2 ++ ")"
versionRangeString (IntersectVersionRanges vr1 vr2) = 
  "(" ++ versionRangeString vr1 ++ ") && (" ++ versionRangeString vr2 ++ ")"
versionRangeString (WildcardVersion v) = "==" ++ versionString v ++ ".*"
versionRangeString _ = "???"