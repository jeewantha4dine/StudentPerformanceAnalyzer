
ghc -package deepseq -package parallel -threaded -O2 -rtsopts -o analyzer src/Main.hs src/DataTypes.hs src/Processing.hs src/IOHandler.hs src/Utils.hs
