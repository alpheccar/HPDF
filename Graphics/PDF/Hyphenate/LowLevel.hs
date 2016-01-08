{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Low level functions for hyphenation
---------------------------------------------------------
-- #hide
module Graphics.PDF.Hyphenate.LowLevel (
   HyphenationDatabase(..)
 , mkExceptions
 , mkPatterns
 , mkCustomLanguage
 )
 where
     
import qualified Graphics.PDF.Data.Trie as T
import  Graphics.PDF.Data.Trie(MapString)
import Data.Char(isDigit)
import Data.List(unfoldr)
import qualified Data.Text as TE

-- | Hyphenation databases
data HyphenationDatabase = English (Maybe (MapString [Int]))
                         | CustomLanguage (MapString [Int]) (MapString [Int])



mkExceptions :: [TE.Text] -> T.MapString [Int]
mkExceptions = T.fromList . map createException
  where
    createException x = (removeHyphen x,exceptionPoints x)
    
mkPatterns :: [TE.Text] -> T.MapString [Int]
mkPatterns = T.fromList . map convertPattern

-- | Create a custom language for hyphenation
mkCustomLanguage :: [TE.Text] -- ^ Exceptions
                 -> [TE.Text] -- ^ Patterns
                 -> HyphenationDatabase
mkCustomLanguage e p = CustomLanguage (mkExceptions e) (mkPatterns p)

-- | Is it a char used in hyphenation pattern
isChar :: Char -> Bool
isChar = not . isDigit

-- | Get numerical value for a char
fromDigit :: Char -> Int
fromDigit c = fromEnum c - fromEnum '0'

-- | Convert a char from an hyphenation pattern to a number
toNumber :: Char -> Int
toNumber x = if isChar x then 0 else fromDigit x

-- | Remove 0 contained between numbers
simplify :: [Int] -> [Int]
simplify (a:b:c:l) | a /= 0 && b == 0 && c /= 0 = a:simplify (c:l)
                   | otherwise = a:simplify (b:c:l)
simplify a = a                
   
-- | Split a patterns into a list of numbers                    
split :: (Char -> Bool) -> TE.Text -> [Int]
split f = simplify . map toNumber . unfoldr (split' f)

split' :: (Char -> Bool) -> TE.Text -> Maybe (Char, TE.Text)
split' f l | TE.null l = Nothing
           | otherwise = if TE.null h then Just (' ', TE.tail t) else Just (TE.head h, t)
    where (h, t) = TE.span f l
  
-- | Convert a pattern into a list of number and a normal word
convertPattern :: TE.Text -> (TE.Text,[Int])
convertPattern s = 
  let s' = TE.filter isChar s
      p =  split isDigit s
  in
  (s',p)
  
-- | Remove hyphens from an excepyion word
removeHyphen :: TE.Text -> TE.Text
removeHyphen = TE.filter ((/=) '-')

-- | Get exception points
exceptionPoints :: TE.Text -> [Int]
exceptionPoints s = 0 : (map onlyHyphen . TE.unpack $ s)
  where
    onlyHyphen '-' = 1
    onlyHyphen _ = 0