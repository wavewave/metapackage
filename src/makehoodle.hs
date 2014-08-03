module Main where

import System.Directory
import System.Environment
import System.FilePath
--
import Driver
import MetaPackage

extralines = "  c-sources: \n\
\                   csrc/c_initdevice.c\n\
\  include-dirs:    csrc\n\
\  install-includes: \n\
\                   csrc/c_initdevice.h\n\
\                   csrc/template-hsc-gtk2hs.h\n\
\                   csrc/XInput.h\n\
\  cc-options:      -Wno-pointer-to-int-cast -std=gnu99 -DDEBUG  -fPIC\n\
\  extra-libraries: X11 Xi dl pthread\n"

testmetaproj base = 
    MetaProject "metahoodle" [ AProject "hoodle-types"   (base </> "hoodle-types")
                             , AProject "xournal-types"  (base </> "xournal-types")
                             , AProject "xournal-parser" (base </> "xournal-parser")
                             , AProject "hoodle-parser"  (base </> "hoodle-parser")
                             , AProject "hoodle-builder" (base </> "hoodle-builder")
                             , AProject "hoodle-render"  (base </> "hoodle-render")
                             , AProject "hoodle-core"    (base </> "hoodle-core")
                             , AProject "hoodle"         (base </> "hoodle") 
                             ]   

copyextra :: FilePath -> FilePath -> IO ()
copyextra base pkgpath = do
    let filelist = map (\x->(base</>"hoodle-core"</>"csrc"</>x , pkgpath</>"csrc"</>x) )
                       [ "c_initdevice.c", "c_initdevice.h", "template-hsc-gtk2hs.h", "XInput.c", "XInput.h" ] 
    createDirectoryIfNotExist (pkgpath </> "csrc")
    mapM_ (uncurry linkFile) filelist

main :: IO () 
main = do 
  args <- getArgs
  putStrLn "makehoodle"
  -- putStrLn extralines
  pkgpath <- makeMetaPackage (testmetaproj (args !! 0)) extralines
  copyextra (args !! 0) pkgpath

