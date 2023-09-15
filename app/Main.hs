module Main where

import BST
import Data.Maybe

main :: IO ()
main =
    let tree = insertAll (node 0) (map node [18, 72, 36, 54, 108]) in 
        print $ inOrder tree
