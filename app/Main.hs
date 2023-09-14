module Main where

import BST
import Data.Maybe

main :: IO ()
main =
    let tree = insert (insert (insert (insert (node 18) (node 36)) (node 30)) (node 1)) (node 1000) in do
        inOrder tree
