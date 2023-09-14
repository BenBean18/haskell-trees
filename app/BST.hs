module BST where

import Data.Maybe

-- Type for a binary search tree node -- contains data and potentially a left and right child
data Node = Node { i :: Int, left :: Maybe Node, right :: Maybe Node } deriving (Ord, Show, Eq)

-- Start at the root.
-- If the node to be added's data <= the current root node's data, move to the left. If there is no left child, add the new node there.
-- If the node to be added's data > the current root node's data, move to the right. If there is no right child, add the new node there.

-- insert root newNode -> newTree
insert :: Node -> Node -> Node
insert root node = 
    if (i node) <= (i root) then
        if isNothing (left root) then Node { i = i root, left = Just node, right = right root }
        else Node { i = i root, left = Just (insert (fromJust $ left root) node), right = right root }
    else
        if isNothing (right root) then Node { i = i root, left = left root, right = Just node }
        else Node { i = i root, left = left root, right = Just (insert (fromJust $ right root) node) }

-- in-order traversal: interact with parent in the middle of children