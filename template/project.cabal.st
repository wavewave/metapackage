Name:		$projname$
Version:	0.0
Synopsis:	
Description: 	
License: 	$licensetype$
License-file:	LICENSE
Author:		Ian-Woo Kim
Maintainer: 	Ian-Woo Kim <ianwookim@gmail.com>
Build-Type: 	Simple
Cabal-Version:  >= 1.8
data-files:     
$datafiles$

Executable $executable$
  Main-is: $executable$.hs
  hs-source-dirs: exe
  ghc-options: 	-Wall -O2 -threaded -funbox-strict-fields -fno-warn-unused-do-bind
  ghc-prof-options: -caf-all -auto-all
  Build-Depends: 
$exedep$

Library
  hs-source-dirs: lib
  ghc-options: 	-Wall -O2 -threaded -funbox-strict-fields -fno-warn-unused-do-bind
  ghc-prof-options: -caf-all -auto-all
  Build-Depends: 
$libdep$
  Exposed-Modules: 
$exposedmodules$
  Other-Modules: 
$othermodule$
 
		 
