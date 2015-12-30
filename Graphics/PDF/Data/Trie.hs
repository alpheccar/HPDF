{-# LANGUAGE CPP #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Trie data structure
---------------------------------------------------------
-- #hide
module Graphics.PDF.Data.Trie(
   MapString(..)
 , lookup
 , insert
 , fromList
 )
 where
    
import Prelude hiding(lookup)
import qualified Data.Map as M

data MapString v = EmptyTrie
                 | Trie (Maybe v) (M.Map Char (MapString v))
                 deriving(Eq,Show)
                 
#if __GLASGOW_HASKELL__ >= 610
myLookup :: Ord k => k -> M.Map k a ->[a]
myLookup k d = case M.lookup k d of
    Just r -> [r]
    _ -> []
#endif
                 
fromList :: [(String,v)] -> MapString v
fromList = foldr addElem EmptyTrie
 where
     addElem (key,v) a = insert key v a
                
lookup :: String
       -> MapString v
       -> [v]
lookup _ EmptyTrie = []
lookup [] (Trie (Just a) _) = [a]
lookup [] (Trie Nothing _) = []
#if __GLASGOW_HASKELL__ >= 610
lookup (c:s) (Trie Nothing tc) = (myLookup c tc >>= lookup s)
lookup (c:s) (Trie (Just tn) tc) = tn:(myLookup c tc >>= lookup s)
#else
lookup (c:s) (Trie Nothing tc) = (M.lookup c tc >>= lookup s)
lookup (c:s) (Trie (Just tn) tc) = tn:(M.lookup c tc >>= lookup s)
#endif
   
insert :: String
       -> v
       -> MapString v
       -> MapString v
insert [] v EmptyTrie = Trie (Just v) M.empty
insert (k:l) v EmptyTrie = Trie Nothing (M.singleton k (insert l v EmptyTrie))

insert [] v (Trie _ b) = Trie (Just v) b
insert (k:s) v (Trie tn tc) = 
  case M.lookup k tc of
    Nothing -> Trie tn (M.insert k (insert s v EmptyTrie) tc)
    Just f -> Trie tn (M.insert k (insert s v f) tc)
    