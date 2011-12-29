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
import Control.Monad hiding (mapM_,msum,forM_)
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

type ProjectPkg = (Project,GenericPackageDescription)


doMetaPkgAction :: (BuildConfiguration -> MetaProject -> [ProjectPkg] -> IO ())
                -> BuildConfiguration -> ProjectConfiguration -> MetaProject -> IO ()
doMetaPkgAction action bc pc mp  = do
  gdescs <- getAllGenPkgDesc bc pc 
  let epkgsdesc = getAllProjPkg gdescs mp 
  either putStrLn (action bc mp) epkgsdesc 


-- | just testing

testMetaPackage :: BuildConfiguration -> ProjectConfiguration -> MetaProject -> IO ()
testMetaPackage = doMetaPkgAction showPkgInfos


-- | starting make job

buildMetaPackage :: BuildConfiguration -> ProjectConfiguration -> MetaProject -> IO ()
buildMetaPackage = doMetaPkgAction makeMetaPackage 

-- | showPkgInfos 

showPkgInfos :: BuildConfiguration -> MetaProject -> [ProjectPkg] -> IO ()
showPkgInfos  bc mp pkgs = do
  let allmodules = getAllModules pkgs  
  (pkgpath,srcpath) <- testInitializeMetaPackage mp
  let allmodnames = do 
        (p,ns) <- allmodules
        (fp,m) <- ns  
        return m

  let allothermodnames = do 
        (_,ns) <- getAllOtherModules pkgs
        (_,m) <- ns 
        return m
  -- mapM_ (putStrLn . show) allothermodnames        
  let lst = getExeFileAndCabalString bc mp "" pkgs 
  
  mapM_ (\(x,y)->do {putStrLn (show x); putStrLn y}) lst

linkExeSrcFile :: (FilePath,FilePath) -> IO ()
linkExeSrcFile (src,dest) = do 
  chk_src <- doesFileExist src
  chk_dest <- doesFileExist dest 
  when (chk_src && not chk_dest) $ do 
    system $ "ln -s " ++ src ++ " " ++ dest
    return ()
  


-- | driver IO action for make a meta package

makeMetaPackage :: BuildConfiguration -> MetaProject 
                -> [ProjectPkg]
                -> IO ()
makeMetaPackage  bc mp pkgs = do
  let allmodules = getAllModules pkgs  
  (pkgpath,srcpath) <- initializeMetaPackage mp
  forM_ (map (projname . fst)  pkgs) $  \x ->   
    system $ "ln -s " ++ bc_progbase bc </> x  ++  " " ++ pkgpath </> "data_" ++ x 

  let allmodnames = do 
        (_,ns) <- allmodules 
        (_,m) <- ns  
        return m

  mapM_ (linkMod srcpath) . concatMap (absolutePathModuleAll bc <$> fst <*> snd) $ allmodules 
  let allothermodnames = do 
        (_,ns) <- getAllOtherModules pkgs
        (_,m) <- ns 
        return m
      allothermodnamestrings = map components allothermodnames
      pathsAction strs = when (take 6 (head strs) == "Paths_") $ do 
                           let pkgname = drop 6 (head strs) 
                           makePaths_xxxHsFile pkgpath mp (ProgProj pkgname)
  mapM_ pathsAction allothermodnamestrings

  let exelst = getExeFileAndCabalString bc mp pkgpath pkgs 
      exestr = concatMap snd exelst 
  mapM_ (linkExeSrcFile . fst) exelst 
  makeCabalFile pkgpath mp pkgs allmodnames allothermodnames exestr

depString :: MetaProject -> [ProjectPkg] -> String 
depString mp pkgs =  
  let depsall = intersectionDeps mp . concatMap getDepsForOnePkg $ pkgs
  in intercalate ",\n" . map (("         " ++) . depFormatter) $ depsall

depFormatter :: Dependency -> String 
depFormatter (Dependency (PackageName pname) vr) = pname ++ versionRangeString vr

replace :: (a -> a) -> (a -> Bool) -> a -> a 
replace f c x = if c x then f x else x 


getTemplate :: IO (STGroup String)
getTemplate = do 
  tmpldir <- getDataDir >>= return . (</> "template")
  directoryGroup tmpldir 
  

-- | create a metapackage cabal file name. 

makeCabalFile :: FilePath -> MetaProject -> [ProjectPkg]
              -> [ModuleName]
              -> [ModuleName]
              -> String
              -> IO ()
makeCabalFile pkgpath mp pkgs modnames othermodnames exestr = do 
  tmpl <- getTemplate
  let mnameformatter = concatMap ("\n          "++) 
                       . map (intercalate "." . components)  
      exposedmodules = mnameformatter modnames
      othermodules   = mnameformatter othermodnames
      libdep = depString mp pkgs 
  let replacement = [ ("projname",metaProjectName mp)
                    , ("licensetype", ""  ) 
                    , ("executables", exestr )
                    , ("libdep", libdep ) 
                    , ("exedep", "")
                    , ("exposedmodules", exposedmodules)
                    , ("othermodules", othermodules)
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
      srcpath = cdir </> metaProjectName mp </> "src"
      exepath = cdir </> (metaProjectName mp) </> "exe"
  createDirectory pkgpath
  createDirectory srcpath 
  createDirectory exepath

  return (pkgpath,srcpath)

-- | just for testing

testInitializeMetaPackage :: MetaProject -> IO (FilePath,FilePath)
testInitializeMetaPackage mp = do 
  cdir <- getCurrentDirectory 
  let pkgpath = cdir </> (metaProjectName mp)
      srcpath = cdir </> (metaProjectName mp </> "src")
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
  chk_hs <- doesFileExist (origfilename <.> "hs") 
  chk_hsc <- doesFileExist (origfilename <.> "hsc")
  chk_chs <- doesFileExist (origfilename <.> "chs")

  if chk_hs 
    then do system $ "ln -s " ++ (origfilename <.> "hs") ++ " " ++ (srcdir </>  toFilePath modname <.> "hs")
            return ()
    else if chk_hsc 
      then do system $ "ln -s " ++ (origfilename <.> "hsc") ++ " " ++ (srcdir </>  toFilePath modname <.> "hsc")
              return ()
      else if chk_chs
        then do system $ "ln -s " ++ (origfilename <.> "chs") ++ " " ++ (srcdir </>  toFilePath modname <.> "chs")
                return ()
        else return ()

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
  
-- | version to string 

versionString :: Version -> String 
versionString (Version b _ ) = intercalate "." (map show b)

-- | version range to string for cabal file

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

-- | get a library 'other module' information for a single package

getOtherModules41Pkg :: ProjectPkg -> [(FilePath,ModuleName)]
getOtherModules41Pkg (proj,desc) = getOtherModules desc

-- | get all other module information

getAllOtherModules :: [ProjectPkg] -> [(Project,[(FilePath,ModuleName)])]
getAllOtherModules pkgs = map ((,) <$> fst <*> getOtherModules41Pkg) pkgs


-- | create Paths_package.hs for a package
 
makePaths_xxxHsFile :: FilePath -> MetaProject -> Project -> IO ()
makePaths_xxxHsFile pkgpath mp proj = do 
  tmpl <- getTemplate
  let pkgname = projname proj 
      datapath = pkgpath </> "data_" ++ pkgname 
      replacement = [ ( "pkgname", pkgname )
                    , ( "datapath", datapath )
                    ] 
      hsstr = renderTemplateGroup tmpl replacement "Paths_xxx.hs" 
  writeFile (pkgpath </> "src" </> "Paths_" ++ pkgname <.> "hs") hsstr

-- | find executables 

getExecutables41Pkg :: MetaProject
                    -> GenericPackageDescription 
                    -> [(String,String,String,String,String)] 
getExecutables41Pkg mp gdesc = do
    (exename,node) <- condExecutables gdesc
    let filename = (modulePath . condTreeData) node
        srcdir = (head . hsSourceDirs . buildInfo . condTreeData) node
        compileopt = intercalate " " . snd . head 
                     . options . buildInfo . condTreeData $ node  
        deps = intercalate ", "
               . map depFormatter 
               . map (replace (const metapkgdep) checkIfInMetaPkg)
               . condTreeConstraints 
               $ node 
    return (exename,filename,srcdir,compileopt,deps)  
  where projnames = map projname . metaProjectPkg $ mp 
        metapkgdep = Dependency (PackageName (metaProjectName mp)) anyVersion
        checkIfInMetaPkg (Dependency (PackageName pname) _) =
            pname `elem` projnames 

 
getExeFileAndCabalString :: BuildConfiguration
                         -> MetaProject
                         -> FilePath
                         -> [ProjectPkg] 
                         -> [((String,String),String)] 
getExeFileAndCabalString bc mp pkgpath pkgs = do 
    p@(proj,desc) <- pkgs
    x@(x1,x2,x3,x4,x5) <- (getExecutables41Pkg mp . snd) p
    let cabalstr = mkExeStrInCabal x  
        filepath = x3</>x2
    return ((bc_progbase bc</>projname proj</>filepath, pkgpath </> "exe" </> x2), cabalstr)


mkExeStrInCabal :: (String,String,String,String,String) -> String
mkExeStrInCabal (exename,filename,srcdir,compileopt,deps) = 
  let tmpl = newSTMP execTemplate :: StringTemplate String 
  in toString . setAttribute "exename" exename
              . setAttribute "srcfilename"  filename  
              . setAttribute "dirname" "exe"
              . setAttribute "option" compileopt
              . setAttribute "dep" deps
              $ tmpl

execTemplate :: String 
execTemplate = "\n\
\Executable $exename$\n\
\  Main-is: $srcfilename$\n\
\  hs-source-dirs: $dirname$\n\
\  ghc-options: $option$\n\
\  Build-Depends: \n\
\                   $dep$\n\n\n"
