{-# LANGUAGE TupleSections #-}

module Driver where


import Control.Applicative ((<$>),(<*>))
import Control.Monad (when)
import Data.Foldable (forM_)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.ModuleName 
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import System.FilePath ((</>))
import System.Process (system)

-- 
import MetaPackage

cabalFile :: AProject -> FilePath 
cabalFile (AProject pname ppath) = ppath </> (pname ++ ".cabal")


parseAProject :: AProject -> IO GenericPackageDescription 
parseAProject proj =
  let cabal = cabalFile proj
  in readPackageDescription normal cabal

-- | relative path info to absolute path info for modules 
absolutePathModule :: AProject -> (FilePath,ModuleName) -> (FilePath,ModuleName)
absolutePathModule proj (fp,modname) = 
  let absolutify dir = projloc proj </> dir
  in (absolutify fp,modname)


{-
getAllGenPkgDesc :: BuildConfiguration -> ProjectConfiguration 
                 -> IO [GenericPackageDescription]
getAllGenPkgDesc bc pc = do 
  let projects = pc_projects pc
  mapM parseAProject projects
-}

{-
doMetaPkgAction :: (BuildConfiguration -> MetaProject -> [ProjectPkg] -> IO ())
                -> BuildConfiguration -> ProjectConfiguration -> MetaProject -> IO ()
doMetaPkgAction action bc pc mp  = do
  gdescs <- getAllGenPkgDesc bc pc 
  let epkgsdesc = getAllProjPkg gdescs mp 
  either putStrLn (action bc mp) epkgsdesc 
-}

-- | driver IO action for make a meta package
makeMetaPackage :: MetaProject -> IO ()
makeMetaPackage mp = do
  parsedpkgs <- mapM (\x -> (x,) <$> parseAProject x) (metaProjectPkg mp)
  let allmodules = getAllModules parsedpkgs  
  (pkgpath,srcpath) <- initializeMetaPackage mp
  -- print allmodules

  forM_ parsedpkgs $  \x ->
    let xname = (projname . fst) x  
        xpath = (projloc . fst) x
    in system $ "ln -s " ++ xpath ++  " " ++ pkgpath </> "data_" ++ xname

  let allmodnames = do 
        (_,ns) <- allmodules 
        (_,m) <- ns  
        return m

  mapM_ (linkMod srcpath) . concatMap (\(proj,lst) -> map (absolutePathModule proj) lst) $ allmodules 


  {- 
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
  -}
