module Data.Markov
  ( Tree (..)
  , probabilities
  , learn
  , learnMany
  , learnNgrams
  , learnManyNgrams
  , predict
  , treeDot
  ) where

import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')
import Data.List (transpose, sortOn)
import System.Random
import Text.Dot
import Control.Monad

type Probability = Double
type Count = Int

-- | Each node stores an associative list to children, from values of s to a's and the actual child.
-- | This type is specialised into count trees and probability trees.
data Tree s v
  = Node (Map.Map s (v, Tree s v))
  deriving (Show, Read)

instance Ord s => Semigroup (Tree s v) where
  Node xs <> Node ys = Node (xs <> ys)

instance Ord s => Monoid (Tree s v) where
  mempty = Node mempty

instance Functor (Tree s) where
  -- fmap :: (a -> b) -> Tree s a -> Tree s b
  fmap f (Node xs) = Node (fmap p xs)
    where p (val, child) = (f val, fmap f child)

-- | Converts a counting tree to a probability tree, where the probabilities in each node sum to 100% and
-- | the probability of each child of a given node is proportional to its count.
probabilities :: Tree s Count -> Tree s Probability
probabilities (Node xs) = Node (fmap update xs)
  where total = sum . fmap fst $ xs
        update (val, child) = (fromIntegral val / fromIntegral total, probabilities child)

-- | Adds a state sequence to the "memory" of a counting tree.
learn :: Ord s => [s] -> Tree s Count -> Tree s Count
learn [] tree = tree
learn (s:ss) tree =
  let Node xs = increment tree s
  in Node (Map.adjust (fmap (learn ss)) s xs)

learnMany :: Ord s => [[s]] -> Tree s Count -> Tree s Count
learnMany xs tree = foldl' (flip learn) tree xs

learnNgrams :: Ord s => Int -> [s] -> Tree s Count -> Tree s Count
learnNgrams n xs = learnMany (ngrams n xs)

learnManyNgrams :: Ord s => Int -> [[s]] -> Tree s Count -> Tree s Count
learnManyNgrams n xs = learnMany (concatMap (ngrams n) xs)

increment :: Ord s => Tree s Count -> s -> Tree s Count
increment (Node xs) s = Node (Map.insertWith (\_ (v, child) -> (v + 1, child)) s (1, Node mempty) xs)

ngrams :: Int -> [a] -> [[a]]
ngrams n xs = transpose [ drop n xs | n <- [0..n-1] ]

findInTree :: Ord s => s -> Tree s a -> Maybe (a, Tree s a)
findInTree s (Node xs) = Map.lookup s xs

-- Follows the given sequence of a's as far as possible down the tree,
-- returning the rest of the tree after the entire sqequence has been
-- followed. If an item in the sequence doesn't exist, the traversal
-- is halted.
follow :: Ord s => [s] -> Tree s a -> Tree s a
follow (s:ss) tree = case findInTree s tree of
  Nothing -> tree
  Just (_, child) -> follow ss child
follow [] tree = tree

predict :: (Ord s, RandomGen g) => g -> [s] -> Tree s Probability -> (s, g)
predict g ss tree = weightedRandom g xs
  where Node child = follow ss tree
        xs = [ (prob, s) | (s, (prob, _)) <- Map.toList child ]

weightedRandom :: (Ord k, Num k, Random k, RandomGen g) => g -> [(k, v)] -> (v, g)
weightedRandom g xs = (select n sorted, g')
  where sorted = sortOn fst xs
        (n, g') = random g
        
        select n [] = error "no possible children to choose from"
        select n [(_, x)] = x
        select n ((n', x):xs)
          | n < n' = x
          | otherwise = select (n - n') xs

treeDot :: (Ord s, Show s, Show a) => String -> Tree s a -> Dot NodeId
treeDot label (Node xs) = do
  id <- node [("label", label)]
  forM_ (Map.toList xs) $ \(s, (v, child)) -> do
    id' <- treeDot (show s) child
    edge id id' [("label", show v)]
  return id
