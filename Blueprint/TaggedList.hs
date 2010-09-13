-- @+leo-ver=4-thin
-- @+node:gcross.20100908110213.1951:@thin TaggedList.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100908110213.1952:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100908110213.1952:<< Language extensions >>
-- @nl

module Blueprint.TaggedList where

-- @<< Import needed modules >>
-- @+node:gcross.20100908110213.1953:<< Import needed modules >>
import Prelude hiding (tail,foldl,foldr,map,mapM,mapM_,length)

import Data.Binary
import Data.Foldable hiding (toList,mapM_)
import Data.Monoid
import Data.Typeable

import TypeLevel.NaturalNumber hiding (NaturalNumber)

import Blueprint.NaturalNumber
-- @-node:gcross.20100908110213.1953:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100908110213.1956:Types
-- @+node:gcross.20100908110213.1957:TaggedList
data TaggedList n a where
    E :: TaggedList Zero a
    (:.) :: a → TaggedList n a → TaggedList (SuccessorTo n) a
  deriving Typeable

infixr :.
-- @-node:gcross.20100908110213.1957:TaggedList
-- @+node:gcross.20100908110213.1990:UntaggedList
data UntaggedList a = ∀ n. UntaggedList (TaggedList n a) deriving Typeable
-- @-node:gcross.20100908110213.1990:UntaggedList
-- @-node:gcross.20100908110213.1956:Types
-- @+node:gcross.20100908110213.1967:Instances
-- @+node:gcross.20100908110213.1996:Binary UntaggedList
instance (NaturalNumber n, Binary a) => Binary (TaggedList n a) where
    get = fmap fromList get
    put = put . toList
-- @-node:gcross.20100908110213.1996:Binary UntaggedList
-- @+node:gcross.20100908110213.2016:Binary UntaggedList
instance Binary a => Binary (UntaggedList a) where
    get = fmap fromListAsUntagged get
    put (UntaggedList l) = put (toList l)
-- @-node:gcross.20100908110213.2016:Binary UntaggedList
-- @+node:gcross.20100908110213.1968:Functor TaggedList
instance Functor (TaggedList Zero) where
    fmap f E = E

instance Functor (TaggedList n) => Functor (TaggedList (SuccessorTo n)) where
    fmap f (x :. xs) = f x :. fmap f xs
-- @-node:gcross.20100908110213.1968:Functor TaggedList
-- @+node:gcross.20100908110213.1969:Foldable TaggedList
instance NaturalNumber n => Foldable (TaggedList n) where
    foldMap f l = go f (length l) l
      where
        go :: Monoid m => (a → m) → N n → TaggedList n a → m
        go f NZero E = mempty
        go f (NSuccessorTo n) (x :. xs) = f x `mappend` go f n xs
-- @-node:gcross.20100908110213.1969:Foldable TaggedList
-- @+node:gcross.20100908110213.2025:Eq
instance (NaturalNumber n, Eq a) => Eq (TaggedList n a) where
    x == y = go (length x) x y
      where
        go :: Eq a => N n → TaggedList n a → TaggedList n a → Bool
        go NZero E E = True
        go (NSuccessorTo n) (x :. xs) (y :. ys) = x == y && go n xs ys
-- @-node:gcross.20100908110213.2025:Eq
-- @-node:gcross.20100908110213.1967:Instances
-- @+node:gcross.20100908110213.1959:Functions
-- @+node:gcross.20100908110213.2026:length
length :: NaturalNumber n => TaggedList n a → N n
length _ = asN
-- @-node:gcross.20100908110213.2026:length
-- @+node:gcross.20100908110213.1960:head
head :: TaggedList (SuccessorTo n) a → a
head (x :. _) = x
-- @-node:gcross.20100908110213.1960:head
-- @+node:gcross.20100908110213.1962:tail
tail :: TaggedList (SuccessorTo n) a → TaggedList n a
tail (_ :. xs) = xs
-- @-node:gcross.20100908110213.1962:tail
-- @+node:gcross.20100908110213.1963:append
append :: TaggedList m a → TaggedList n a → TaggedList (Plus m n) a
append E = id
append (x :. xs) = (x :.) . append xs
-- @-node:gcross.20100908110213.1963:append
-- @+node:gcross.20100908110213.1976:join
join :: TaggedList m a → TaggedList n a → (TaggedList (Plus m n) a,TaggedList (Plus m n) b → (TaggedList m b,TaggedList n b))
join E v = (v,\z → (E,z))
join (x :. xs) v =
    let (vv,split) = join xs v 
    in (x :. vv
       ,(\(y :. ys) → let (a,b) = split ys in (y :. a,b))
       )
-- @-node:gcross.20100908110213.1976:join
-- @+node:gcross.20100908110213.1970:replace
replace :: [a] → TaggedList n b → TaggedList n a
replace [] E = E
replace [] _ = error "There are not enough elements in the list to replace all elements of the tagged list."
replace (x:xs) (_:.ys) = x :. replace xs ys
replace (x:xs) E = error "There are too many elements in the list to replace all elements of the tagged list."
-- @-node:gcross.20100908110213.1970:replace
-- @+node:gcross.20100908110213.1971:unwrapRights
unwrapRights :: TaggedList n (Either a b) → Either [a] (TaggedList n b)
unwrapRights E = Right E
unwrapRights (Left x :. rest) =  
    case unwrapRights rest of
        Left xs → Left (x:xs)
        Right _ → Left [x]
unwrapRights (Right x :. rest) =  
    case unwrapRights rest of
        Left es → Left es
        Right xs → Right (x:.xs)
-- @-node:gcross.20100908110213.1971:unwrapRights
-- @+node:gcross.20100908110213.1972:map
map :: (a → b) → TaggedList n a → TaggedList n b
map f E = E
map f (x :. xs) = f x :. map f xs
-- @-node:gcross.20100908110213.1972:map
-- @+node:gcross.20100908110213.1973:toList
toList :: TaggedList n a → [a]
toList E = []
toList (x :. xs) = x : toList xs
-- @-node:gcross.20100908110213.1973:toList
-- @+node:gcross.20100908110213.1991:fromListAsUntagged
fromListAsUntagged :: [a] → UntaggedList a
fromListAsUntagged [] = UntaggedList E
fromListAsUntagged (x:rest) =
    case fromListAsUntagged rest of
        UntaggedList xs → UntaggedList (x :. xs)
-- @-node:gcross.20100908110213.1991:fromListAsUntagged
-- @+node:gcross.20100908110213.2017:fromList
fromList :: NaturalNumber n => [a] → TaggedList n a
fromList = go asN
  where
    go :: N n → [a] → TaggedList n a
    go NZero [] = E
    go (NSuccessorTo m) (x:xs) = x :. go m xs
    go (NSuccessorTo _) [] = error "List is too short to convert into a tagged list of the given length."
    go NZero (_:_) = error "List is too long to convert into a tagged list of the given length."
-- @-node:gcross.20100908110213.2017:fromList
-- @+node:gcross.20100908110213.1995:eqLists
eqLists :: Eq a => TaggedList m a → TaggedList n a → Bool
eqLists E E = True
eqLists E _ = False
eqLists _ E = False
eqLists (x:.xs) (y:.ys) = (x == y) && eqLists xs ys
-- @-node:gcross.20100908110213.1995:eqLists
-- @+node:gcross.20100908110213.2021:mapM
mapM :: Monad m => (a → m b) → TaggedList n a → m (TaggedList n b)
mapM f E = return E
mapM f (x :. xs) = do
    y ← f x
    ys ← mapM f xs
    return (y :. ys)
-- @-node:gcross.20100908110213.2021:mapM
-- @+node:gcross.20100908110213.2023:mapM_
mapM_ :: Monad m => (a → m b) → TaggedList n a → m ()
mapM_ f E = return ()
mapM_ f (x :. xs) = f x >> mapM_ f xs
-- @-node:gcross.20100908110213.2023:mapM_
-- @+node:gcross.20100908110213.2024:zipf
zipf :: TaggedList n (a → b) → TaggedList n a → TaggedList n b
zipf E E = E
zipf (f :. fs) (x :. xs) = f x :. zipf fs xs
-- @-node:gcross.20100908110213.2024:zipf
-- @-node:gcross.20100908110213.1959:Functions
-- @-others
-- @-node:gcross.20100908110213.1951:@thin TaggedList.hs
-- @-leo
