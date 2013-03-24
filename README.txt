TO BUILD AND INSTALL THE LIBRARY
================================

ghc-pkg unregister HPDF-0.x if you already installed an older version.
ghc-pkg list to check which version is already installed.

runghc Setup.hs configure
runghc Setup.hs build
runghc Setup.hs haddock to generate again the HTML pages (optional - only if you have haddock installed)
runghc Setup.hs install (you may have to use sudo)
runghc Setup.hs clean (warning : will clean the HTML pages)

TO TEST THE LIBRARY
=====================
cd Test
make demo         : to build a demo pdf
./test            : to run the demo