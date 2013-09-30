{-# LANGUAGE NoImplicitPrelude #-}
module Language.Spaceship.GbTree
   ( module Data.Map
   , gbTreeFromList
   , gbTreeNull
   , GbTree()
   ) where

import Prelude ( Ord(..) , Bool )

import Data.Map (
     delete -- delete_any - doesnt assume present - if not present, ignore
   , empty -- empty - new empty tree
   , insert -- enter - inserts into tree but doesnt assume present
   , (!) -- get - assumes present in tree
   , member -- is_defined - key is present in tree
   , keys -- keys - returns keys in order
   , mapWithKey -- map :: (k -> v -> v) -> Map k v -> Map k v
   , size -- size
   , toList -- to_list
   , elems -- values -- like keys
   )
import qualified Data.Map as M (fromList, null, Map())

   -- , fromList -- have to implement this as:
              -- "gb_trees:from_orddict(orddict:from_list(Foo))" -- de-dupes
gbTreeFromList :: (Ord k) => [(k, a)] -> GbTree k a
gbTreeFromList = M.fromList

   -- , null -- is_empty - check if tree is empty
gbTreeNull :: GbTree k a -> Bool
gbTreeNull = M.null

type GbTree k a = M.Map k a
