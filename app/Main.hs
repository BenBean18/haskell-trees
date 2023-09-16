module Main where

import BST
import Data.Maybe

bstSort :: (Ord t) => [t] -> [t]
bstSort = inOrder . tree

main :: IO ()
main =
--     let tree = fromJust $ removeAll (insertAll (node 1000) (map node [500,1500,1400,1600,1410,1420,1430,1450,1440])) (map node [1500]) in do
--         print tree
--         print $ inOrder tree
    print $ bstSort [1000,500,1500,1400,1600,1410,1420,1430,1450,1440]

{-

                        1000
    500                                         1500
                                        1400            1600
                                                1410
                                                        1420
                                                                1430
                                                                        1450
                                                                1440


-}