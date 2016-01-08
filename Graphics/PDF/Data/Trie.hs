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
import qualified Data.Text as T

data MapString v = EmptyTrie
                 | Trie (Maybe v) (M.Map Char (MapString v))
                 deriving(Eq,Show)
                 
myLookup :: Ord k => k -> M.Map k a ->[a]
myLookup k d = case M.lookup k d of
    Just r -> [r]
    _ -> []
                 
fromList :: [(T.Text,v)] -> MapString v
fromList = foldr addElem EmptyTrie
 where
     addElem (key,v) a = insert key v a
                
lookup :: T.Text
       -> MapString v
       -> [v]
lookup _ EmptyTrie = []
lookup t (Trie (Just tn) tc) | T.null t = [tn]
                             | otherwise = 
                                  let c = T.head t 
                                      s = T.tail t
                                  in 
                                  tn:(myLookup c tc >>= lookup s)

lookup t  (Trie Nothing tc) | T.null t = []
                            | otherwise = 
                                  let c = T.head t 
                                      s = T.tail t
                                  in 
                                  myLookup c tc >>= lookup s

   
insert :: T.Text
       -> v
       -> MapString v
       -> MapString v
insert t v EmptyTrie | T.null t = Trie (Just v) M.empty
                     | otherwise = 
                          let k = T.head t 
                              l = T.tail t
                          in 
                          Trie Nothing (M.singleton k (insert l v EmptyTrie))

insert t v (Trie tn tc) | T.null t = Trie (Just v) tc
                        | otherwise = 
                            let k = T.head t 
                                s = T.tail t
                            in 
                            case M.lookup k tc of
                              Nothing -> Trie tn (M.insert k (insert s v EmptyTrie) tc)
                              Just f -> Trie tn (M.insert k (insert s v f) tc)
    