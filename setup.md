## setup
1. Install ghcup from here: https://www.haskell.org/ghcup/install/ Links to an external site.

2. Edit the .cabal file in the FORTH directory so that build-depends has   build-depends: base >=4.10 && <4.18, flow >=1.0.19

3. Ran "cabal install" (completed successfully) (also probably need to create an empty ChangeLog.md file)

4. Attempted to run "cabal install hbase" as instructed in the README. This does not complete successfully and returns the following error: Error: [Cabal-7127] Cannot build the package hbase, it is not in this project (either directly or indirectly). If you want to add it to the project then edit the cabal.project file. (I cannot find this in hackage so just proceeded and ignored this)

5. Ran "cabal build"

6. To run ValSpec.hs, EvalSpec.hs, and InterpreSpec.hs I needed to run "cabal install hspec --lib", "cabal install quickcheck --lib"(QuickCheck), and "cabal install flow --lib". Then I could run the runhaskell <HSpec test file> commands successfully.
