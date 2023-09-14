module Main where

import BST
import Data.Maybe

main :: IO ()
main =
    let tree = insert (insert (Node { i = 18, left = Nothing, right = Nothing }) (Node { i = 36, left = Nothing, right = Nothing })) (Node { i = 30, left = Nothing, right = Nothing }) in do
        print tree
