{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- AFM Parser
---------------------------------------------------------
module Graphics.PDF.Fonts.AFMParser(
    getFont
    ) where 

import Text.ParserCombinators.Parsec hiding(space)
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char hiding(space)
import Data.Char(toUpper)
import System.Environment
import Data.Maybe(isJust,fromJust,mapMaybe)
import qualified Data.IntMap as IM
import Data.List(intersperse)
import qualified Data.Map.Strict as M
import Graphics.PDF.Fonts.Font(FontStructure(..),emptyFontStructure,FontSize,GlyphSize,GlyphPair(..))
import Paths_HPDF
import Graphics.PDF.LowLevel.Types

capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : t

line :: Parser ()
line = do string "\r\n"
          return ()

toEndOfLine :: Parser ()
toEndOfLine = do many (noneOf "\r\n")
                 line
                 return ()
                 
getString :: String -> Parser String
getString s = do 
               string s
               spaces
               c <- many1 (alphaNum <|> oneOf "-+")
               line
               return c
               
getName :: String -> Parser String
getName s = do 
              string s
              spaces
              c <- alphaNum >> many (alphaNum <|> oneOf " -+")
              line
              return c
               
getInt :: String -> Parser Int
getInt s = do c <- getString s
              return $ read c
              
getFloat :: String -> Parser Double
getFloat s = do string s
                spaces
                c <- many1 (alphaNum <|> oneOf ".-+")
                line
                return $ read c
              
getBool :: String -> Parser Bool
getBool s = do c <- getString s
               return $ read (capitalize c)
               
data CharacterSet = ExtendedRoman
                  | Special
                  deriving(Eq,Read,Show)
    
data Weight = Medium
            | Bold
            | Roman
            deriving(Eq,Read,Show)
               
getCharacterSet :: String -> Parser CharacterSet
getCharacterSet s = do c <- getString s
                       return $ read c
                       
getWeigth :: String -> Parser Weight
getWeigth s = do c <- getString s
                 return $ read c
                 
  
array :: Parser [String]  
array = sepEndBy (many1 (oneOf "-+0123456789")) (many1 (oneOf " "))
                 
getArray :: String -> Parser [Double]
getArray s = do string s
                spaces
                c <- array
                line
                return . map read $ c
                 
comment :: String -> Parser ()
comment s = do string s
               toEndOfLine
             
comments :: Parser ()
comments = do many1 (comment "Comment")
              return ()
              
data EncodingScheme = AdobeStandardEncoding 
                    | FontSpecific
                    deriving(Eq,Read,Show)
                
getEncoding :: String -> Parser EncodingScheme
getEncoding s = do c <- getString s
                   return $ read c
                                           
number :: Parser Int
number  = do c <- many1 (oneOf "-+0123456789")
             return $ read c
         
data Elem = C Int
          | WX Int
          | N String
          | B [Double]
          | L
          deriving(Eq,Read,Show)    
               
metricElem :: Parser Elem
metricElem  = do char 'C'
                 spaces
                 c <- number 
                 return $ C c
              <|>
              do string "WX"
                 spaces
                 c <- number   
                 return $ WX c
              <|> 
              do char 'N'
                 spaces
                 c <- many1 alphaNum
                 return $ N c
              <|>
              do char 'B'
                 spaces
                 c <- array
                 return . B . map read $ c   
              <|> 
              do char 'L'
                 spaces
                 many1 letter
                 spaces
                 many1 letter
                 return L
                 
data Metric = Metric { charCode :: Int
                     , metricWidth :: Int
                     , name :: String
                     , bounds :: [Double]
                     }
                     deriving(Eq,Show)
                     
data AFMFont = AFMFont { metrics :: [Metric]
                       , underlinePosition :: Int
                       , underlineThickness :: Int
                       , ascent :: Int
                       , afmDescent :: Int
                       , kernData :: Maybe [KX]
                       }
                       deriving(Eq,Show)
                        
isEncoded :: Metric -> Bool
isEncoded (Metric c _ _ _) = c /= (-1)                  
                        
mkMetric :: [Elem] -> Metric
mkMetric l = foldr addElem (Metric (-1) 0 "" []) l     
 where
     addElem  (C c) m = m {charCode=c}
     addElem  (WX c) m = m {metricWidth=c}
     addElem  (N s) m = m {name=s}
     addElem  (B l) m = m {bounds=l}
     addElem  _ m = m         
                          
charMetric :: Parser Metric
charMetric = do
       l <- sepEndBy metricElem (many1 (oneOf "; ")) 
       line 
       return . mkMetric $ l
       
data KX = KX String String Int  
        deriving(Eq,Ord,Show)  
       
kernPair :: Parser KX
kernPair = do string "KPX"
              spaces
              namea <- many1 letter
              spaces
              nameb <- many1 letter
              spaces
              nb <- many1 (oneOf "-+0123456789")
              line
              return $ KX namea nameb (read nb)
                       
getKernData :: Parser (Maybe [KX])
getKernData = do 
            { comment "StartKernData"
            ; nbKerns <- getInt "StartKernPairs" 
            ; k <- many1 kernPair
            ; comment "EndKernPairs"
            ; comment "EndKernData"
            ; return $ Just k
            }
                            
afm :: Parser AFMFont
afm = do { comment "StartFontMetrics"
         ; comments
         ; fontName <- getString "FontName"
         ; fullName <- getName "FullName"
         ; familyName <- getString "FamilyName"
         ; weight <- getWeigth "Weight"
         ; italicAngle <- getFloat "ItalicAngle"
         ; isFixedPitch <- getBool "IsFixedPitch"
         ; characterSet <- getCharacterSet "CharacterSet"
         ; bbox <- getArray "FontBBox"
         ; underlinePosition <- getInt "UnderlinePosition"
         ; underlineThickness <- getInt "UnderlineThickness"
         ; comment "Version"
         ; comment "Notice"
         ; encodingScheme <- getEncoding "EncodingScheme"
         ; capHeight <- option 0 $ getInt "CapHeight"
         ; xHeight <- option 0 $ getInt "XHeight"
         ; ascender <- option 0 $ getInt "Ascender"
         ; descender <- option 0 $ getInt "Descender" 
         ; stdHW <- getInt "StdHW"
         ; stdVW <- getInt "StdVW"
         ; startCharMetrics <- getInt "StartCharMetrics"
         ; charMetrics <- many1 charMetric
         ; comment "EndCharMetrics"
         ; kerns <- option Nothing getKernData
         ; comment "EndFontMetrics"
         ; return $ AFMFont (filter isEncoded charMetrics) underlinePosition underlineThickness ascender descender kerns
         }


addMetric :: Metric -> FontStructure -> FontStructure 
addMetric m fs = 
    let fs' = fs { width = M.insert (fromIntegral $ charCode m) (fromIntegral $ metricWidth m) (width fs)}
    in 
    case (name m) of 
      "space" -> fs' {space = fromIntegral $ charCode m}
      "hyphen" -> fs' {hyphen = Just (fromIntegral $ charCode m)}
      _ -> fs'

addKern :: M.Map String GlyphCode -> KX -> FontStructure -> FontStructure 
addKern d (KX sa sb c) fs = 
  let caM = M.lookup sa d 
      cbM = M.lookup sb d
  in 
  case (caM,cbM) of
    (Just ca, Just cb) -> fs {kern = M.insert (GlyphPair ca cb) (fromIntegral c) (kern fs)}
    _ -> fs

fontToStructure :: AFMFont -> FontStructure 
fontToStructure afm =
  let h = (ascent afm - afmDescent afm) 
      fs = emptyFontStructure { descent = fromIntegral $ - (afmDescent afm)
                              , height = fromIntegral $ h 
                              }
      addName m d = M.insert (name m) (fromIntegral $ charCode m) d 
      nameToGlyph = foldr addName M.empty (metrics afm)
      fs1 = foldr addMetric fs (metrics afm)
  in
  case kernData afm of
    Nothing -> fs1
    Just k -> foldr (addKern nameToGlyph) fs1 k

              
parseFont :: String -> IO (Maybe AFMFont)
parseFont s = do
    path <- getDataFileName s
    r <- parseFromFile afm path
    case r of
      Left e -> error (show e)
      Right r -> return $ Just r

getFont :: String -> IO (Maybe FontStructure)
getFont s = do 
  result <- parseFont s 
  case result of 
    Nothing -> return Nothing 
    Just r -> return (Just $ fontToStructure r)

