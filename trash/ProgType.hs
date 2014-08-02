{-# LANGUAGE DeriveDataTypeable #-}

module Application.MetaPackage.ProgType where 

import System.Console.CmdArgs

data Metapackage = Test 
                 | Make 
              deriving (Show,Data,Typeable)

test :: Metapackage
test = Test 

make :: Metapackage 
make = Make

mode = modes [test,make]

