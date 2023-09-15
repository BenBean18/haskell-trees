module Main where

import BST
import Data.Maybe

main :: IO ()
main =
    let tree = fromJust $ remove (insert (insert (insert (insert (node 1) (node 0)) (node 5)) (node 3)) (node 8)) (node 1) in 
        print $ inOrder tree
