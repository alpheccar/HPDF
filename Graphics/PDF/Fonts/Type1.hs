{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
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
{-# LANGUAGE FlexibleContexts #-}
module Graphics.PDF.Fonts.Type1(
      IsFont
    , GlyphSize
    , Type1Font
    , AFMData
    , Type1FontStructure(..)
    , getAfmData
    , mkType1FontStructure
) where 

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import Data.Char 
import qualified Data.Map.Strict as M
import Graphics.PDF.Fonts.Font
import Graphics.PDF.Fonts.AFMParser 
import System.FilePath 
import Graphics.PDF.Fonts.Encoding
import Graphics.PDF.Fonts.FontTypes
import Graphics.PDF.Fonts.AFMParser (AFMFont, parseFont)
import Data.List 
import Data.Function(on)

instance IsFont Type1Font where 
  getDescent (Type1Font fs _) s = trueSize s $ descent fs 
  getHeight (Type1Font fs _) s = trueSize s $ height fs 
  getKern (Type1Font fs _) s a b = trueSize s $ M.findWithDefault 0 (GlyphPair a b) (kernMetrics fs)
  glyphWidth (Type1Font fs _) s a = trueSize s  $ M.findWithDefault 0 a (widthData fs)
  charGlyph (Type1Font fs _) c = M.findWithDefault 0 c (encoding fs)
  name (Type1Font fs _) = baseFont fs 
  hyphenGlyph (Type1Font fs _) = hyphen fs 
  spaceGlyph (Type1Font fs _) = space fs

data AFMData = AFMData AFMFont 
data Type1FontStructure = Type1FontStructure FontData FontStructure

getAfmData :: FilePath -> IO AFMData 
getAfmData path = do  
    Just r <- parseFont (Right path) 
    return (AFMData r)

mkType1FontStructure :: FontData -> AFMData -> IO (Maybe Type1FontStructure)
mkType1FontStructure pdfRef (AFMData f)  = do
  theEncoding <- getEncoding AdobeStandardEncoding
  maybeFs <- getFont (Right f) theEncoding Nothing
  case maybeFs of 
    Just theFont -> 
      return . Just $ Type1FontStructure pdfRef theFont
    Nothing -> return Nothing

 

instance PdfResourceObject Type1Font where
   toRsrc (Type1Font f ref) =  
   	        AnyPdfObject . PDFDictionary . M.fromList $
                           [(PDFName "Type",AnyPdfObject . PDFName $ "Font")
                           , (PDFName "Subtype",AnyPdfObject . PDFName $ "Type1")
                           , (PDFName "BaseFont",AnyPdfObject . PDFName $ baseFont f)
                           , (PDFName "FirstChar",AnyPdfObject . PDFInteger $ (fromIntegral firstChar))
                           , (PDFName "LastChar",AnyPdfObject . PDFInteger $ (fromIntegral lastChar))
                           , (PDFName "Widths",AnyPdfObject  $ widths)
                           , (PDFName "FontDescriptor", AnyPdfObject descriptor)
                           ] 
          where 
            codes = map fst . M.toList $ widthData f
            firstChar = head . sort $ codes
            lastChar = head . reverse . sort $ codes
            widths = map (AnyPdfObject . PDFInteger . fromIntegral . snd) . sortBy (compare `on` fst) . M.toList $ widthData f
            bbox = map AnyPdfObject .fontBBox $ f 
            descriptor = PDFDictionary . M.fromList $ 
          	  [ (PDFName "Type",AnyPdfObject . PDFName $ "Font")
              , (PDFName "Subtype",AnyPdfObject . PDFName $ "Type1")
              , (PDFName "BaseFont",AnyPdfObject . PDFName $ baseFont f)
              , (PDFName "FontFile", AnyPdfObject ref)
              , (PDFName "Flags",AnyPdfObject . PDFInteger . fromIntegral . mkFlags $ f)
              , (PDFName "FontBBox",AnyPdfObject  $ bbox)
              , (PDFName "ItalicAngle",AnyPdfObject $ italicAngle f)
              , (PDFName "Ascent",AnyPdfObject . PDFInteger . fromIntegral $ ascent f)
              , (PDFName "Descent",AnyPdfObject . PDFInteger . fromIntegral $ descent f)
              , (PDFName "CapHeight",AnyPdfObject . PDFInteger . fromIntegral $ capHeight f)
          	  ]
