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


hyphenToUnderscore :: String -> String
hyphenToUnderscore = map (\x -> if x == '-' then '_' else x)

-- | driver IO action for make a meta package
makeMetaPackage :: MetaProject -> String -> IO FilePath
makeMetaPackage mp extra = do
  parsedpkgs <- mapM (\x -> (x,) <$> parseAProject x) (metaProjectPkg mp)
  let allmodules = getAllModules parsedpkgs  
  (pkgpath,srcpath) <- initializeMetaPackage mp

  forM_ parsedpkgs $  \x ->
    let xname = (projname . fst) x  
        xpath = (projloc . fst) x
    in linkDirectory xpath (pkgpath </> "data_" ++ (hyphenToUnderscore xname))

  mapM_ (linkMod srcpath) . concatMap (\(proj,lst) -> map (absolutePathModule proj) lst) $ allmodules 

  let allmodnames      = do (_,ns) <- allmodules 
                            (_,m) <- ns  
                            return m
      allothermodnames = do (_,ns) <- getAllOtherModules parsedpkgs
                            (_,m) <- ns 
                            return m
      allothermodnamestrings = map components allothermodnames
      pathsAction strs = when (take 6 (head strs) == "Paths_") $ do 
                           let pname = drop 6 (head strs) 
                           makePaths_xxxHsFile pkgpath mp pname
  mapM_ pathsAction (allothermodnamestrings ++ map components allmodnames)
  
  {-
  let exelst = getExeFileAndCabalString bc mp pkgpath pkgs 
      exestr = concatMap snd exelst 
  mapM_ (linkExeSrcFile . fst) exelst 
  -}

  makeCabalFile pkgpath mp parsedpkgs allmodnames allothermodnames extra -- "" -- exestr
  
  return pkgpath
