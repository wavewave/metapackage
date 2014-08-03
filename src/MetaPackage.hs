module MetaPackage where

import Control.Applicative
import Control.Monad hiding (mapM_,msum,forM_)
import Data.Foldable
import Data.Function
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.List (intercalate,group,sort,sortBy)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.ModuleName
import Distribution.Version

import System.Directory
import System.FilePath
import System.Process
import Text.StringTemplate
import Text.StringTemplate.Helpers
--
import Application.DevAdmin.Cabal
--
import Paths_metapackage
import Prelude hiding (foldr1,foldr, mapM_, concatMap, concat, sum, elem)

data AProject = AProject { projname :: String
                         , projloc :: FilePath }
              deriving Show
 
type AProjectParsed = (AProject,GenericPackageDescription)

data MetaProject = MetaProject { metaProjectName :: String
                               , metaProjectPkg :: [AProject]  
                               }
                 deriving (Show)


createDirectoryIfNotExist :: FilePath -> IO ()
createDirectoryIfNotExist fp = do
    putStrLn fp
    doesDirectoryExist fp >>= \b -> when (not b) (createDirectory fp)

-- | 
linkFile :: FilePath -> FilePath -> IO ()
linkFile src tgt = do
    doesFileExist tgt >>= \b -> when b (removeFile tgt)
    system ("ln -s " ++ src ++ " " ++ tgt) >> return ()

-- | 
linkDirectory :: FilePath -> FilePath -> IO ()
linkDirectory src tgt = do
    doesDirectoryExist tgt >>= \b -> when b (removeFile tgt)
    system ("ln -s " ++ src ++ " " ++ tgt) >> return ()


linkExeSrcFile :: (FilePath,FilePath) -> IO ()
linkExeSrcFile (src,dest) = do 
  chk_src <- doesFileExist src
  chk_dest <- doesFileExist dest 
  when (chk_src && not chk_dest) $ do 
    linkFile src dest    
    -- system $ "ln -s " ++ src ++ " " ++ dest
    return ()
  
depString :: MetaProject -> [AProjectParsed] -> String 
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
makeCabalFile :: FilePath -> MetaProject -> [AProjectParsed]
              -> [ModuleName]
              -> [ModuleName]
              -> String -- ^ extra
              -> IO ()
makeCabalFile pkgpath mp pkgs modnames othermodnames extra {- exestr -} = do 
  tmpl <- getTemplate
  let mnameformatter = concatMap ("\n          "++) 
                       . map (intercalate "." . components)  
      exposedmodules = mnameformatter modnames
      othermodules   = mnameformatter othermodnames
      libdep = depString mp pkgs 
  let replacement = [ ("projname",metaProjectName mp)
                    , ("licensetype", ""  ) 
                    -- , ("executables", exestr )
                    , ("libdep", libdep ) 
                    , ("exedep", "")
                    , ("exposedmodules", exposedmodules)
                    , ("othermodules", othermodules)
                    , ("modulebase", "src")
                    , ("progtype", "") 
                    , ("extralines", extra)
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
  createDirectoryIfNotExist pkgpath
  createDirectoryIfNotExist srcpath 
  createDirectoryIfNotExist exepath

  return (pkgpath,srcpath)

-- | just for testing
testInitializeMetaPackage :: MetaProject -> IO (FilePath,FilePath)
testInitializeMetaPackage mp = do 
  cdir <- getCurrentDirectory 
  let pkgpath = cdir </> (metaProjectName mp)
      srcpath = cdir </> (metaProjectName mp </> "src")
  return (pkgpath,srcpath)


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
    then do linkFile (origfilename <.> "hs") (srcdir </>  toFilePath modname <.> "hs")
            return ()
    else if chk_hsc 
      then do linkFile (origfilename <.> "hsc") (srcdir </>  toFilePath modname <.> "hsc")
              return ()
      else if chk_chs
        then do linkFile (origfilename <.> "chs") (srcdir </>  toFilePath modname <.> "chs")
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
modDirName mname = let initpart = (init . components) mname
                   in if null initpart 
                        then "" 
                        else (toFilePath . fromString . intercalate "." ) initpart 



-- | get Project and Generic Package Description
getProjPkg :: M.Map String GenericPackageDescription -> AProject  
           -> Either String AProjectParsed
getProjPkg namepkgmap proj = 
  let mpkgdesc = M.lookup (projname proj) namepkgmap 
  in maybe (Left ("package " ++ projname proj ++ " doesn't exist"))
           (Right . (,) proj ) 
           mpkgdesc      
  
-- | get a library pkg information for all packages in the metapackage
getAllProjPkg :: [GenericPackageDescription] -> MetaProject 
              -> Either String [AProjectParsed]
getAllProjPkg gdescs mp = 
  let namepkgpair = map ((,) <$> getPkgName <*> id) gdescs 
      namepkgmap = M.fromList namepkgpair
      pkgs = metaProjectPkg mp
  in mapM (getProjPkg namepkgmap) pkgs 


-- | get a library module information for a single package
getModulesForOnePkg :: AProjectParsed -> [(FilePath,ModuleName)]
getModulesForOnePkg (proj,desc) = getModules desc 
  

-- | get all library module information
getAllModules :: [AProjectParsed] -> [(AProject,[(FilePath,ModuleName)])] 
getAllModules pkgs = map ((,) <$> fst <*> getModulesForOnePkg) pkgs 


-- | get a library dep information 
getDepsForOnePkg :: AProjectParsed -> [Dependency]
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
getOtherModules41Pkg :: AProjectParsed -> [(FilePath,ModuleName)]
getOtherModules41Pkg (proj,desc) = getOtherModules desc

-- | get all other module information
getAllOtherModules :: [AProjectParsed] -> [(AProject,[(FilePath,ModuleName)])]
getAllOtherModules pkgs = map ((,) <$> fst <*> getOtherModules41Pkg) pkgs

-- | create Paths_package.hs for a package
makePaths_xxxHsFile :: FilePath -> MetaProject -> String -> IO ()
makePaths_xxxHsFile pkgpath mp pname = do 
  tmpl <- getTemplate
  let datapath = pkgpath </> "data_" ++ pname 
      replacement = [ ( "pkgname", pname )
                    , ( "datapath", datapath )
                    ] 
      hsstr = renderTemplateGroup tmpl replacement "Paths_xxx.hs" 
  writeFile (pkgpath </> "src" </> "Paths_" ++ pname <.> "hs") hsstr

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

-- |
mkExeStrInCabal :: (String,String,String,String,String) -> String
mkExeStrInCabal (exename,filename,srcdir,compileopt,deps) = 
  let tmpl = newSTMP execTemplate :: StringTemplate String 
  in toString . setAttribute "exename" exename
              . setAttribute "srcfilename"  filename  
              . setAttribute "dirname" "exe"
              . setAttribute "option" compileopt
              . setAttribute "dep" deps
              $ tmpl

-- |
execTemplate :: String 
execTemplate = "\n\
\Executable $exename$\n\
\  Main-is: $srcfilename$\n\
\  hs-source-dirs: $dirname$\n\
\  ghc-options: $option$\n\
\  Build-Depends: \n\
\                   $dep$\n\n\n"
