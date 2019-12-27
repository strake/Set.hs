{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Data.Set.Class where

import Prelude hiding (Foldable (..), filter)
import Control.Monad (guard)
import qualified Data.Foldable as F
import Data.Function (on)
import qualified Data.Maybe as List (catMaybes, mapMaybe)
import Data.Monoid
import qualified Data.Set as C
import Util

class Set set where
    type Elem set
    empty :: set
    fold :: Monoid (Elem set) => set -> Elem set
    foldMap :: Monoid a => (Elem set -> a) -> set -> a
    foldr :: (Elem set -> a -> a) -> a -> set -> a
    foldl :: (a -> Elem set -> a) -> a -> set -> a
    toList :: set -> [Elem set]
    length :: set -> Int
    null :: set -> Bool
    mapMaybe :: (Elem set -> Maybe (Elem set)) -> set -> set
    mapEither :: (Elem set -> Either (Elem set) (Elem set)) -> set -> (set, set)
    filter :: (Elem set -> Bool) -> set -> set
    member :: Elem set -> set -> Bool
    fromList :: [Elem set] -> set
    insert, delete :: Elem set -> set -> set
    singleton :: Elem set -> set
    union, intersection, difference, symmetricDifference :: set -> set -> set
    (⊆) :: set -> set -> Bool

    fold = foldMap id
    foldMap f = foldr ((<>) . f) mempty
    foldr f z = flip appEndo z . foldMap (Endo . f)
    foldl f z = flip appEndo z . getDual . foldMap (Dual . Endo . flip f)
    toList = foldr (:) []
    length = F.length . toList
    null = (==) 0 . length
    fromList = F.foldr insert empty
    member = (⊆) . singleton
    (⊆) = null ∘∘ difference
    union = fromList ∘∘ (++) `on` toList
    difference = foldr delete
    symmetricDifference = \ a b -> difference a b `union` difference b a
    intersection = \ a b -> union a b `difference` symmetricDifference a b
    mapMaybe f = fromList . List.mapMaybe f . toList
    mapEither f = (,) <$> mapMaybe (either Just (pure Nothing) . f) <*> mapMaybe (either (pure Nothing) Just . f)
    filter p = mapMaybe ((<$) <*> guard . p)

instance Ord a => Set (C.Set a) where
    type Elem (C.Set a) = a
    empty = C.empty
    fold = F.fold
    foldMap = F.foldMap
    foldr = C.foldr
    foldl = C.foldl
    toList = C.toList
    length = C.size
    null = C.null
    mapMaybe f = C.fromAscList . List.catMaybes . C.toAscList . C.map f
    filter = C.filter
    member = C.member
    fromList = C.fromList
    insert = C.insert
    delete = C.delete
    singleton = C.singleton
    union = C.union
    intersection = C.intersection
    difference = C.difference
    (⊆) = C.isSubsetOf
