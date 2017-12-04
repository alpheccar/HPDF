---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Font
---------------------------------------------------------
module Graphics.PDF.Typesetting.WritingSystem(
      WritingSystem(..)
    , Language(..)
    , mapToSpecialGlyphs
) where 

import qualified Data.Text as T
import Graphics.PDF.LowLevel.Types
import qualified Text.Hyphenation as H
import Data.List(intersperse)
import Data.Char 
import Data.List(unfoldr)

data Language = English
              | German
              | OtherLanguage

data WritingSystem = Latin Language
                   | UnknownWritingSystem


myWords' :: T.Text -> Maybe (T.Text, T.Text)
myWords' l  | T.null l = Nothing
            | otherwise = if T.null h then Just (h', t') else Just (T.singleton ' ', t)
    where 
        (h, t) = T.span isSpace l
        (h', t') = T.span (not . isSpace) l
 
 
-- | Split a sentence into words keeping the space but shortening them to 1 space
myWords :: T.Text -> [T.Text]     
myWords l = concatMap onlyWord . unfoldr myWords' $ l 
 where
  onlyWord s = 
     let (w,p) = T.span isAlpha s in
     case (T.null w,T.null p) of
         (True,True) -> []
         (False,True) -> [w]
         (True,False) -> [p]
         (False,False) -> [w,p]

addHyphens :: H.Hyphenator -> T.Text -> T.Text
addHyphens hn f =
    T.concat . map (T.concat . intersperse (T.pack "/-") . hyphenate) . myWords $ f
  where
    hyphenate = fmap T.pack . H.hyphenate hn . T.unpack


mapGlyphsGerEng :: H.Hyphenator -> T.Text -> [SpecialChar]
mapGlyphsGerEng hn theText =
  let getBreakingGlyphs [] = []
      getBreakingGlyphs (a:'/':'-':d:l) = (NormalChar a):BreakingHyphen:getBreakingGlyphs (d:l)
      getBreakingGlyphs (',':' ':l) = NormalChar ',':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs (';':' ':l) = NormalChar ';':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs ('.':' ':l) = NormalChar '.':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs (':':' ':l) = NormalChar ':':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs ('!':' ':l) = NormalChar '!':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs ('?':' ':l) = NormalChar '?':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs (' ':l) = NormalSpace:getBreakingGlyphs l
      getBreakingGlyphs (a:l) = NormalChar a:getBreakingGlyphs l
  in getBreakingGlyphs (T.unpack . addHyphens hn $ theText)

mapToSpecialGlyphs :: WritingSystem -> T.Text -> [SpecialChar]
mapToSpecialGlyphs UnknownWritingSystem theText = 
  let getBreakingGlyphs (' ':l) = NormalSpace:getBreakingGlyphs l 
      getBreakingGlyphs (a:l) = NormalChar a:getBreakingGlyphs l 
      getBreakingGlyphs [] = []
  in getBreakingGlyphs (T.unpack theText)
mapToSpecialGlyphs (Latin OtherLanguage) theText = 
  let getBreakingGlyphs (' ':l) = NormalSpace:getBreakingGlyphs l 
      getBreakingGlyphs (a:l) = NormalChar a:getBreakingGlyphs l 
      getBreakingGlyphs [] = []
  in getBreakingGlyphs (T.unpack theText)
mapToSpecialGlyphs (Latin German) theText = mapGlyphsGerEng H.german_1996 theText
mapToSpecialGlyphs (Latin English) theText = mapGlyphsGerEng H.english_US theText
