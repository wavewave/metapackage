

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
      allothermodnames = do 
        (_,ns) <- getAllOtherModules pkgs
        (_,m) <- ns 
        return m
      lst = getExeFileAndCabalString bc mp "" pkgs 
  mapM_ (\(x,y)->do {putStrLn (show x); putStrLn y}) lst



-- | for all modules 
absolutePathModuleAll :: BuildConfiguration -> Project 
                         -> [(FilePath,ModuleName)] -> [(FilePath,ModuleName)]
absolutePathModuleAll bc proj xs = 
  map (absolutePathModule bc proj) xs 




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
    return ((bc_srcbase bc</>projname proj</>filepath, pkgpath </> "exe" </> x2), cabalstr)
