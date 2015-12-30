---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Hyphenate a string
---------------------------------------------------------
module Graphics.PDF.Hyphenate(
 -- * Type
   HyphenationDatabase(..)
 , MapString
 -- * Hyphenation databases
 , mkCustomLanguage
 , mkExceptions
 -- * Hyphenation
 , hyphenate
 ) where

import qualified Graphics.PDF.Data.Trie as T
import qualified Graphics.PDF.Hyphenate.English as E
import Graphics.PDF.Data.Trie(MapString)
import Graphics.PDF.Hyphenate.LowLevel
import Data.Char(toLower)

exceptions :: HyphenationDatabase -> T.MapString [Int]
exceptions (English _) = E.exceptions
exceptions (CustomLanguage e _) = e

addedExceptions :: HyphenationDatabase -> T.MapString [Int]
addedExceptions (English (Just e)) = e
addedExceptions _ = T.EmptyTrie
 
patterns :: HyphenationDatabase -> T.MapString [Int]
patterns (English _) = E.patterns
patterns (CustomLanguage _ p) = p
    
-- | Get the hyphen positions for a word
getWordPoints  :: HyphenationDatabase -> String -> [Int]
getWordPoints db s = 
    case (T.lookup (map toLower s) (exceptions db)) ++ (T.lookup (map toLower s) (addedExceptions db)) of
        [] -> let s' =  '.':s ++ "." in
          getFromPattern db s' 
        l -> head l
        
-- | Get the hyphen positions from the patterns
getFromPattern :: HyphenationDatabase -> String -> [Int]
getFromPattern db s = 
    let startPoints = map (const 0) $ s
        lookP _ [] = []
        lookP c x = 
            let r = T.lookup x (patterns db)
            in
              (map ((++) c) r) ++ lookP (0:c) (tail x)
        
        foundPoints = reverse . lookP [] $ s
        onlyMax a b = zipWith max (a ++ repeat 0) b 
    in
     foldr onlyMax startPoints foundPoints
        
-- | Hyphenate a string
hyphenate :: HyphenationDatabase -- ^ Hyphenation database to use to hyphenate the word
          -> String -- ^ Word to hyphenate
          -> [String]
hyphenate db s = 
    let p = 0:0:(drop 2. drop 1 . lastPointIsNull . getWordPoints db $ s)
        lastPointIsNull l = let (_,t) = splitAt 2 (reverse l) in reverse (0:0:t)
        cutFromList c [] = [reverse c]
        cutFromList c ((ch,pnb):l) = 
            if pnb `mod` 2 == 1 
              then if null c 
                     then cutFromList [ch] l 
                     else reverse c : cutFromList [ch] l 
              else cutFromList (ch:c) l
    in
     if length s <= 4 then [s] else cutFromList [] (zip s p)
    
