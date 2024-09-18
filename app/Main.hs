module Main (main) where

import qualified Lib
import Types.Mod

main :: IO ()
main = Lib.run (ParseRepoGraph "input.json")
