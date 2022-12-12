module Graph where

import Data.List (delete)

{- There exists 3 implementations of a graph that we will consider. A graph
    using adjacency lists, adjacency matrix and a linked network
-}

newtype Graph a = Graph
    { internal :: [Node a]
    }

data Node a = Node
    { key       :: Int
    , adjacent  :: [Int]
    , value     :: a
    }
    deriving (Eq)

newtype Edge = Edge
    { edge :: (Int, Int)
    }

instance Functor Node where
    fmap f (Node k ks x) = Node k ks . f $ x

instance Functor Graph where
    fmap f = Graph . map (fmap f) . internal

empty :: Graph a
empty = Graph []

-- ensures that all keys with a node are unique
addNode :: Int -> a -> Graph a -> Graph a
addNode k val = Graph . insertOrdered k val [] . internal
    where
        -- we ensure stored keys are ordered
        insertOrdered :: Int -> a -> [Node a] -> [Node a] -> [Node a]
        insertOrdered k val acc [] = Node k [] val : acc
        insertOrdered k val acc (x:xs)
          | key x == k  = (++) xs . flip (:) acc . Node k [] $ val  -- replace
          | key x > k   = (++) xs . (:) x . flip (:) acc . Node k [] $ val  -- insert
          | otherwise   = insertOrdered k val (x:acc) xs    -- recurse

-- unsafe implementation
insertEdge :: Edge -> Graph a -> Graph a
insertEdge e = Graph . map (addEdge e) . internal
  where
    addEdge :: Edge -> Node a -> Node a
    addEdge e (Node k ks val)
      | u == k    = Node k (v : ks) val
      | v == k    = Node k (u : ks) val
      | otherwise = Node k ks val
      where (u,v) = edge e

removeEdge :: Edge -> Graph a -> Graph a
removeEdge e = Graph . map (rmvEdge e) . internal
  where
    rmvEdge :: Edge -> Node a -> Node a
    rmvEdge e (Node k ks val)
      | u == k    = Node k (delete v ks) val
      | v == k    = Node k (delete u ks) val
      | otherwise = Node k ks val
      where (u,v) = edge e


-- check if a graph is simple
-- O(|e|)
isSimple :: Graph a -> Bool
isSimple = all isSimple' . internal
  where
    isSimple' :: Node a -> Bool
    isSimple' (Node k ks _) = (isUnique k ks) && (not . elem k $ ks)

    -- uses the assumption of a sorted list
    isUnique :: (Eq a) => a -> [a] -> Bool
    isUnique _ [] = True
    isUnique prev (x:xs)
      | prev == x = False
      | otherwise = isUnique x xs

-- get the underlying simple graph of a graph
-- O(|e|)
simple :: Graph a -> Graph a
simple = Graph . map simple' . internal
  where
    simple' :: Node a -> Node a
    simple' (Node k ks val) =
      let ks' = snd . foldr unique (k, []) . filter (/= k) $ ks
       in Node k ks' val

    -- uses the assumption of a sorted list
    unique :: (Eq a) => a -> (a, [a]) -> (a, [a])
    unique x (prev, xs)
      | x == prev = (x, xs)
      | otherwise = (x, x:xs)

-- given the ordered nature of edges we can simply check if graphs are identical
instance (Eq a) => Eq (Graph a) where
  (==) (Graph g) (Graph h)
    = all (uncurry (==)) . zip g $ h

-- determine if two graphs are identical
-- O(|e| + |v|)
isIdentical :: (Eq a) => Graph a -> Graph a -> Bool
isIdentical g h = g == h

-- determine if two graphs are isomorphic
-- maybe we can write a function that will reconstruct a graph in a consistent
-- way. Then we can just check if the reconstructions are identical
isIsomorphic :: (Eq a) => Graph a -> Graph a -> Bool
isIsomorphic g h = isIdentical (reconstruct g) (reconstruct h)

reconstruct :: Graph a -> Graph a
reconstruct = undefined
