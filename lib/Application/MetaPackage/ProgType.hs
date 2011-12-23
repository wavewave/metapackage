{-# LANGUAGE DeriveDataTypeable #-}

module Application.MetaPackage.ProgType where 

import System.Console.CmdArgs

data Metapackage = Test 
              deriving (Show,Data,Typeable)

test :: Metapackage
test = Test 

mode = modes [test]

