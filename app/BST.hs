module BST where

import Data.Maybe

-- Type for a binary search tree node -- contains data and potentially a left and right child
data Node t = Node { i :: t, left :: Maybe (Node t), right :: Maybe (Node t) } deriving (Ord, Show, Eq)

-- Start at the root.
-- If the node to be added's data <= the current root node's data, move to the left. If there is no left child, add the new node there.
-- If the node to be added's data > the current root node's data, move to the right. If there is no right child, add the new node there.

-- insert root newNode -> newTree
insert :: Ord t => Node t -> Node t -> Node t
insert root node
  | (i node) <= (i root) = if isNothing (left root) then Node { i = i root, left = Just node, right = right root }
        else Node { i = i root, left = Just (insert (fromJust $ left root) node), right = right root }
  | isNothing (right root) = Node { i = i root, left = left root, right = Just node }
  | otherwise = Node { i = i root, left = left root, right = Just (insert (fromJust $ right root) node) }

-- in-order traversal: interact with parent in the middle of children
inOrder :: Node t -> [t]
inOrder Node { i = i, left = Nothing, right = Nothing } = [i]
inOrder node =
    (if isJust $ left node then inOrder $ fromJust (left node) else []) ++
    [i node] ++
    (if isJust $ right node then inOrder $ fromJust (right node) else [])

node :: i -> Node i
node i = Node { i = i, left = Nothing, right = Nothing }