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
      allothermodnames = do 
        (_,ns) <- getAllOtherModules pkgs
        (_,m) <- ns 
        return m
      lst = getExeFileAndCabalString bc mp "" pkgs 
  mapM_ (\(x,y)->do {putStrLn (show x); putStrLn y}) lst

-- | driver IO action for make a meta package

makeMetaPackage :: BuildConfiguration -> MetaProject 
                -> [ProjectPkg]
                -> IO ()
makeMetaPackage  bc mp pkgs = do
  let allmodules = getAllModules pkgs  
  (pkgpath,srcpath) <- initializeMetaPackage mp
  forM_ (map (projname . fst)  pkgs) $  \x ->   
    system $ "ln -s " ++ bc_srcbase bc </> x  ++  " " ++ pkgpath </> "data_" ++ x 

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



-- | for all modules 
absolutePathModuleAll :: BuildConfiguration -> Project 
                         -> [(FilePath,ModuleName)] -> [(FilePath,ModuleName)]
absolutePathModuleAll bc proj xs = 
  map (absolutePathModule bc proj) xs 


-- | relative path info to absolute path info for modules 
absolutePathModule :: BuildConfiguration -> Project 
                   -> (FilePath,ModuleName) -> (FilePath,ModuleName)
absolutePathModule bc proj (fp,modname) = 
  let absolutify dir = bc_srcbase bc </> projname proj </> dir
  in (absolutify fp,modname)


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
