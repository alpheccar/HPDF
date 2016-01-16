{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- AFM AFMParser
---------------------------------------------------------
module Graphics.PDF.Fonts.AFMParser(
      getFont
    , AFMFont(..)
    , EncodingScheme(..)
    , Metric(..)
    , KX(..)
    , parseFont
    ) where 

import Text.ParserCombinators.Parsec hiding(space)
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char hiding(space)
import Text.Parsec(modifyState,getState)
import Text.Parsec.Prim(parserZero)
import Data.Char(toUpper)
import System.Environment
import Data.Maybe(isJust,fromJust,mapMaybe)
import qualified Data.IntMap as IM
import Data.List(intersperse)
import qualified Data.Map.Strict as M
import Graphics.PDF.Fonts.Font(emptyFontStructure)
import Paths_HPDF
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Fonts.Encoding(PostscriptName)
import Graphics.PDF.Fonts.FontTypes
import Control.Monad.State

data Metric = Metric { charCode :: Int
                     , metricWidth :: Int
                     , name :: String
                     , bounds :: [Double]
                     }
                     deriving(Eq,Show)
                     
data EncodingScheme = AFMAdobeStandardEncoding 
                    | AFMFontSpecific
                    | AFMUnsupportedEncoding
                    deriving(Eq,Read,Show)

data KX = KX String String Int  
        deriving(Eq,Ord,Show)  

data AFMFont = AFMFont { metrics :: [Metric]
                       , underlinePosition :: Int
                       , underlineThickness :: Int
                       , afmAscent :: Int
                       , afmDescent :: Int
                       , kernData :: Maybe [KX]
                       , type1BaseFont :: String
                       , encodingScheme :: EncodingScheme
                       , afmItalic :: Double 
                       , afmCapHeight :: Int
                       , afmBBox :: [Double]
                       , afmFixedPitch :: Bool
                       , afmSymbolic :: Bool
                       }
                       deriving(Eq,Show)


type AFMParser = GenParser Char AFMFont

emptyAFM = AFMFont { metrics = []
                   , underlinePosition = 0
                   , underlineThickness = 0
                   , afmAscent = 0
                   , afmDescent = 0
                   , kernData = Nothing
                   , type1BaseFont = ""
                   , encodingScheme = AFMAdobeStandardEncoding
                   , afmItalic = 0.0
                   , afmCapHeight = 0
                   , afmBBox = []
                   , afmFixedPitch = False
                   , afmSymbolic = False
                   }
                    
capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : t


line :: AFMParser ()
line = do string "\r\n" <|> string "\n"
          return ()

toEndOfLine :: AFMParser ()
toEndOfLine = do many (noneOf "\r\n")
                 line
                 return ()
                 
getString :: AFMParser String
getString = do 
  c <- many1 (alphaNum <|> oneOf "-+")
  line
  return c

getSentence :: AFMParser String
getSentence = do 
               c <- many1 (alphaNum <|> oneOf " -+")
               line
               return c

            
getName :: AFMParser String
getName = do 
              c <- alphaNum >> many (alphaNum <|> oneOf " -+")
              line
              return c
               
getInt :: AFMParser Int
getInt  = do c <- getString
             return $ read c
              
getFloat :: AFMParser Double
getFloat = do 
                c <- many1 (alphaNum <|> oneOf ".-+")
                line
                return $ read c
              
getBool :: AFMParser Bool
getBool = do c <- getString
             return $ read (capitalize c)
               
data CharacterSet = ExtendedRoman
                  | Special
                  deriving(Eq,Read,Show)
    
data Weight = Medium
            | Bold
            | Roman
            deriving(Eq,Read,Show)
               
getCharacterSet :: AFMParser CharacterSet
getCharacterSet = do c <- getString
                     return $ read c
                       
getWeigth :: AFMParser Weight
getWeigth = do c <- getString
               return $ read c
                 
  
array :: AFMParser [String]  
array = sepEndBy (many1 (oneOf "-+0123456789")) (many1 (oneOf " "))
                 
getArray :: AFMParser [Double]
getArray  = do c <- array
               line
               return . map read $ c
                 

           
getEncoding :: AFMParser EncodingScheme
getEncoding = do 
  c <- getString
  case c of 
    "AdobeStandardEncoding" -> return AFMAdobeStandardEncoding
    "FontSpecific" -> return AFMFontSpecific 
    _ -> return  AFMUnsupportedEncoding     
                                           
number :: AFMParser Int
number  = do c <- many1 (oneOf "-+0123456789")
             return $ read c
         
data Elem = C Int
          | WX Int
          | N String
          | B [Double]
          | L
          deriving(Eq,Read,Show)    
               
metricElem :: AFMParser Elem
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
                          
charMetric :: AFMParser Metric
charMetric = do
       l <- sepEndBy metricElem (many1 (oneOf "; ")) 
       line 
       return . mkMetric $ l
       

       
kernPair :: AFMParser KX
kernPair = do string "KPX"
              spaces
              namea <- many1 letter
              spaces
              nameb <- many1 letter
              spaces
              nb <- many1 (oneOf "-+0123456789")
              line
              return $ KX namea nameb (read nb)
                       

              
keyword :: String -> AFMParser () -> AFMParser () 
keyword s action = do 
  string s
  spaces
  action
  return ()

anyKeyWord :: AFMParser () 
anyKeyWord = do 
  many1 alphaNum
  spaces 
  toEndOfLine

header :: String -> AFMParser () 
header s = do 
  string s  
  toEndOfLine 
  return ()

notHeader :: String -> AFMParser () 
notHeader s = do 
  r <- many1 alphaNum
  if s == r 
    then 
      parserZero 
    else do 
      toEndOfLine

specific :: AFMParser () 
specific = choice [ try $ keyword "FontName" (getString >>= \name -> modifyState $ \afm -> afm {type1BaseFont = name}) 
                  , try $ keyword "UnderlinePosition" (getInt >>= \name -> modifyState $ \afm -> afm {underlinePosition = name}) 
                  , try $ keyword "UnderlineThickness" (getInt >>= \name -> modifyState $ \afm -> afm {underlineThickness = name})
                  , try $ keyword "EncodingScheme" (getEncoding >>= \name -> modifyState $ \afm -> afm {encodingScheme = name})
                  , try $ keyword "CapHeight" (getInt >>= \name -> modifyState $ \afm -> afm {afmCapHeight = name}) 
                  , try $ keyword "Ascender" (getInt >>= \name -> modifyState $ \afm -> afm {afmAscent = name})
                  , try $ keyword "Descender" (getInt >>= \name -> modifyState $ \afm -> afm {afmDescent = name}) 
                  , try $ keyword "ItalicAngle" (getFloat >>= \name -> modifyState $ \afm -> afm {afmItalic = name}) 
                  , try $ keyword "IsFixedPitch" (getBool >>= \name -> modifyState $ \afm -> afm {afmFixedPitch = name}) 
                  , try $ keyword "FontBBox" (getArray >>= \name -> modifyState $ \afm -> afm {afmBBox = name}) 
                  , try $ notHeader "StartCharMetrics"
                  ]

getKernData :: AFMParser (Maybe [KX])
getKernData = do 
            { header "StartKernData"
            ; header "StartKernPairs" 
            ; k <- many1 kernPair
            ; header "EndKernPairs"
            ; header "EndKernData"
            ; return $ Just k
            }

afm :: AFMParser AFMFont
afm = 
  do  
    header "StartFontMetrics"
    many1 specific 
    header "StartCharMetrics"
    charMetrics <- many1 charMetric
    header "EndCharMetrics"
    kerns <- option Nothing getKernData
    string "EndFontMetrics"
    
    modifyState $ \afm -> afm { metrics = charMetrics 
                              , kernData = kerns
                              }

    afm <- getState 
    let [xmin,ymin,xmax,ymax] = afmBBox afm
    if afmAscent afm == 0 
    then
       let h = floor (ymax - ymin) in
       return $ afm { afmAscent = h 
                    , afmDescent = 0 
                    }
    else
       return $ afm

addMetric :: M.Map PostscriptName GlyphCode -> Metric -> FontStructure -> FontStructure 
addMetric nameToGlyph m fs = 
    let c = M.lookup (name m) nameToGlyph
        fs' = case c of 
                Just glyphCode -> 
                  fs { widthData = M.insert (fromIntegral glyphCode) (fromIntegral $ metricWidth m) (widthData fs)}
                Nothing -> fs
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
    (Just ca, Just cb) -> fs {kernMetrics = M.insert (GlyphPair ca cb) (fromIntegral c) (kernMetrics fs)}
    _ -> fs

-- If the maybe argument is not nothing, we use the specific encoding for
-- the postscript names.
-- Otherwise we use the encoding we found in the afm file.
-- It is used to force MacRomanEncoding on not symbolic default fonts.
fontToStructure :: AFMFont 
                -> M.Map PostscriptName Char 
                -> Maybe (M.Map PostscriptName GlyphCode)
                -> FontStructure 
fontToStructure afm encoding maybeMapNameToGlyph =
  let h = (afmAscent afm - afmDescent afm) 
      fs = emptyFontStructure { descent = fromIntegral $ - (afmDescent afm)
                              , height = fromIntegral $ h 
                              , ascent = fromIntegral $ afmAscent afm
                              , fontBBox = afmBBox afm
                              , italicAngle = afmItalic afm
                              , capHeight = fromIntegral $ afmCapHeight afm
                              , fixedPitch = afmFixedPitch afm
                              , serif = False
                              , symbolic = afmSymbolic afm
                              , script = False
                              , nonSymbolic = not (afmSymbolic afm)
                              , italic = False
                              , allCap = False
                              , smallCap = False
                              , forceBold = False
                              }
      addName m d | charCode m == -1 = d
                  | otherwise = M.insert (name m) (fromIntegral $ charCode m) d 
      nameToGlyph = maybe (foldr addName M.empty (metrics afm)) id maybeMapNameToGlyph
      fs1 = foldr (addMetric nameToGlyph) fs (metrics afm)
      addEncodingMapping (pname,glyphcode) d = 
         let unicodeM = M.lookup pname encoding 
         in 
         case unicodeM of 
          Nothing -> d 
          Just code -> M.insert code glyphcode d 
      mapping = foldr addEncodingMapping M.empty (M.toList nameToGlyph)
      fs2 = fs1 { encoding = mapping}
  in
  case kernData afm of
    Nothing -> fs2
    Just k -> foldr (addKern nameToGlyph) fs2 k

afmParseFromFile :: AFMParser AFMFont -> FilePath -> IO (Either ParseError AFMFont)
afmParseFromFile p path = do 
  l <- readFile path 
  return $ runParser p emptyAFM path l

parseFont :: Either String String -> IO (Maybe AFMFont)
parseFont (Left s) = do
    path <- getDataFileName s
    r <- afmParseFromFile afm path
    case r of
      Left e -> error (show e)
      Right r -> return $ Just r
parseFont (Right path) = do
    r <- afmParseFromFile afm path
    case r of
      Left e -> error (show e)
      Right r -> return $ Just r

getFont :: Either String AFMFont
        -> M.Map PostscriptName Char  -- ^ Glyph name to unicode
        -> Maybe (M.Map PostscriptName GlyphCode)  -- ^ Glyph name to glyph code if not standard coding
        -> IO (Maybe FontStructure)
getFont (Left s) encoding nameToGlyph = do 
  result <- parseFont (Left s) 
  case result of 
    Nothing -> return Nothing 
    Just r -> return (Just $ fontToStructure r encoding nameToGlyph)
getFont (Right result) encoding nameToGlyph = return . Just $ fontToStructure result encoding nameToGlyph

