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
import qualified Data.IntSet as I
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
    size :: set -> Int
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
    size = F.length . toList
    null = (==) 0 . size
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
{-# DEPRECATED length "use `size`" #-}

instance Ord a => Set (C.Set a) where
    type Elem (C.Set a) = a
    empty = C.empty
    fold = F.fold
    foldMap = F.foldMap
    foldr = C.foldr
    foldl = C.foldl
    toList = C.toList
    length = C.size
    size = C.size
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

instance Set I.IntSet where
    type Elem I.IntSet = Int
    empty = I.empty
    foldr = I.foldr
    foldl = I.foldl
    toList = I.toList
    length = I.size
    size = I.size
    null = I.null
    filter = I.filter
    member = I.member
    fromList = I.fromList
    insert = I.insert
    delete = I.delete
    singleton = I.singleton
    union = I.union
    intersection = I.intersection
    difference = I.difference
    (⊆) = I.isSubsetOf

newtype Union set = Union { unUnion :: set }
  deriving (Eq, Ord)

instance Set set => Semigroup (Union set) where
    Union as <> Union bs = Union (union as bs)

instance Set set => Monoid (Union set) where
    mempty = Union empty

newtype Intersection set = Intersection { unIntersection :: set }
  deriving (Eq, Ord)

instance Set set => Semigroup (Intersection set) where
    Intersection as <> Intersection bs = Intersection (intersection as bs)

newtype SymmetricDifference set = SymmetricDifference { unSymmetricDifference :: set }
  deriving (Eq, Ord)

instance Set set => Semigroup (SymmetricDifference set) where
    SymmetricDifference as <> SymmetricDifference bs = SymmetricDifference (symmetricDifference as bs)

instance Set set => Monoid (SymmetricDifference set) where
    mempty = SymmetricDifference empty
