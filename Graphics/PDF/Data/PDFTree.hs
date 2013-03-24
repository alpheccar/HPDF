{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoBangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  PDFTree.hs
-- Copyright   :  (c) Daan Leijen 2002
-- License     :  BSD-style
-- Maintainer  :  misc@NOSPAMalpheccar.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from integer keys to values.
--
-- Customized by alpheccar for the need of the PDF library. The original is IntMap from
-- the ghc standard libraries
-----------------------------------------------------------------------------
-- #hide
module Graphics.PDF.Data.PDFTree(
   PDFTree
 , Key
 , empty
 , lookup
 , insert
 , fromList
 , fold2
 , isLeaf
 , size
 , keyOf
 ) where
 
import Prelude hiding (lookup,map,filter,foldr,foldl,null)
import Data.Bits
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts ( Word(..), Int(..), shiftRL# )
#elif __GLASGOW_HASKELL__
import Word
import GlaExts ( Word(..), Int(..), shiftRL# )
#else
import Data.Word
#endif

import Graphics.PDF.LowLevel.Types

type Nat = Word

natFromInt :: Key a -> Nat
natFromInt (PDFReference i) = fromIntegral i

intFromNat :: Nat -> Key a
intFromNat w = PDFReference (fromIntegral w)

type Prefix a = PDFReference a
type Mask   a = PDFReference a
type Key a   = PDFReference a

-- | A map of integers to values @a@.
-- The total size of subtrees is tracked by each node. It is needed for the PDF Tree
data PDFTree a = Nil
               | Tip {-# UNPACK #-} !(Key a) a
               | Bin {-# UNPACK #-} !(Prefix a) {-# UNPACK #-} !(Mask a) !(PDFTree a) !(PDFTree a) 
               deriving(Eq,Show)
               
-- | The key function needed to export a Tree of PDF objects into the format defined 
-- by the PDF spec
fold2 :: Monad m => Maybe b -- ^ Parent ref
      -> (Maybe b -> PDFTree a -> PDFTree a -> m (Int,b)) -- ^ Node action
      -> (Maybe b -> Key a -> a -> m (Int,b)) -- ^ Leaf action
      -> PDFTree a -- ^ PDFTree
      -> m (Int,b) -- ^ Final action and reference of the root node
fold2 _ _ _ Nil = error "Page tree is empty"
fold2 p _ leaf (Tip k a) = leaf p k a
fold2 p node _ (Bin _ _ l r) = node p l r



isLeaf :: PDFTree a -> Bool
isLeaf (Tip _ _) = True
isLeaf _ = False

keyOf :: PDFTree a -> Key a
keyOf (Tip k _) = k
keyOf _ = error "No key for a node"
  
{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(n)/. Number of elements in the map.
size :: PDFTree a -> Int
size t
  = case t of
      Bin _ _ l r -> (size l) + (size r)
      Tip _ _ -> 1
      Nil     -> 0

-- | /O(min(n,W))/. Lookup the value at a key in the map.
lookup :: (Monad m) => Key a -> PDFTree a -> m a
lookup k t = case lookup' k t of
    Just x -> return x
    Nothing -> fail "Data.PDFTree.lookup: Key not found"

lookup' :: Key a -> PDFTree a -> Maybe a
lookup' k t
  = let nk = natFromInt k  in seq nk (lookupN nk t)

lookupN :: Nat -> PDFTree a -> Maybe a
lookupN k t
  = case t of
      Bin _ m l r 
        | zeroN k (natFromInt m) -> lookupN k l
        | otherwise              -> lookupN k r
      Tip kx x 
        | (k == natFromInt kx)  -> Just x
        | otherwise             -> Nothing
      Nil -> Nothing
      
zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

insert :: Key a -> a -> PDFTree a -> PDFTree a
insert k x t
  = case t of
      Bin p m l r 
        | nomatch k p m -> join k (Tip k x) p t
        | zero k m      -> Bin p m (insert k x l) r
        | otherwise     -> Bin p m l (insert k x r)
      Tip ky _ 
        | k==ky         -> Tip k x
        | otherwise     -> join k (Tip k x) ky t
      Nil -> Tip k x
      
join :: Prefix a -> PDFTree a -> Prefix a -> PDFTree a -> PDFTree a
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
    
zero :: Key a -> Mask a -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0
  
nomatch :: Key a -> Prefix a -> Mask a -> Bool
nomatch i p m
  = (mask i m) /= p

mask :: Key a -> Mask a -> Prefix a
mask i m
  = maskW (natFromInt i) (natFromInt m)
  
{--------------------------------------------------------------------
  Big endian operations  
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix a
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))

branchMask :: Prefix a -> Prefix a -> Mask a
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
  
highestBitMask :: Nat -> Nat
highestBitMask x
  = case (x .|. shiftRL x 1) of 
     x1 -> case (x1 .|. shiftRL x1 2) of 
      x2 -> case (x2 .|. shiftRL x2 4) of 
       x3 -> case (x3 .|. shiftRL x3 8) of 
        x4 -> case (x4 .|. shiftRL x4 16) of 
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
          x6 -> (x6 `xor` (shiftRL x6 1))
          
shiftRL :: Nat -> Int -> Nat
#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
shiftRL (W# x) (I# i)
  = W# (shiftRL# x i)
#else
shiftRL x i   = shiftR x i
#endif

empty :: PDFTree a
empty
  = Nil
  
{--------------------------------------------------------------------
  Utilities 
--------------------------------------------------------------------}
foldlStrict :: (a -> t -> a) -> a -> [t] -> a
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)
      
-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
fromList :: [(Key a,a)] -> PDFTree a
fromList xs
  = foldlStrict ins empty xs
  where
    ins t (k,x)  = insert k x t
    

  