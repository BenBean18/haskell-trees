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
  | i node <= i root = if isNothing (left root) then Node { i = i root, left = Just node, right = right root }
        else Node { i = i root, left = Just (insert (fromJust $ left root) node), right = right root }
  | isNothing (right root) = Node { i = i root, left = left root, right = Just node }
  | otherwise = Node { i = i root, left = left root, right = Just (insert (fromJust $ right root) node) }

-- Returns (parent, child)
search :: Ord t => Node t -> Node t -> Maybe (Node t, Node t)
search root node
    | i node < i root = if isJust (left root) then search (fromJust $ left root) node else Nothing
    | i node == i root = Just (root, node)
    | i node > i root = if isJust (right root) then search (fromJust $ right root) node else Nothing

-- plan out remove function!
-- if node to remove == root
    -- If it has zero or one children, set the parent's reference to the node to its existent child (or Nothing if no children)
    -- If it has two children, go one to the left and all the way to the right (greatest value that is less than current).
    -- Copy that node's data to the node to be removed, then remove that node all the way on the right.
-- if node to remove < root: go left
-- if node to remove > root: go right
remove :: Ord t => Node t -> Node t -> Maybe (Node t)
remove root node
  | i node < i root = if isJust (left root) then Just Node { i = i root, left = remove (fromJust $ left root) node, right = right root } else Just root
  | i node > i root = if isJust (right root) then Just Node { i = i root, left = left root, right = remove (fromJust $ right root) node } else Just root
  | i node == i root =
    if isJust (left root) && isJust (right root) then
        -- two children
        let (newLeft, value) = removeReturnFarRight $ fromJust $ left root in
            Just Node { i = value, left = newLeft, right = right root }
    else if isJust $ left root then left root
    else right root

-- if there is no right node (this is the last node), return (Nothing, i root)
removeReturnFarRight :: Ord t => Node t -> (Maybe (Node t), t)
removeReturnFarRight root
  | isNothing (right root) = (Nothing, i root)
  | otherwise = (Just Node { i = i root, left = left root, right = fst $ removeReturnFarRight (fromJust $ right root) }, snd $ removeReturnFarRight (fromJust $ right root))

-- in-order traversal: interact with parent in the middle of children
inOrder :: Node t -> [t]
inOrder Node { i = i, left = Nothing, right = Nothing } = [i]
inOrder node =
    (if isJust $ left node then inOrder $ fromJust (left node) else []) ++
    [i node] ++
    (if isJust $ right node then inOrder $ fromJust (right node) else [])

node :: i -> Node i
node i = Node { i = i, left = Nothing, right = Nothing }