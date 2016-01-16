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
module Graphics.PDF.Fonts.StandardFont(
      IsFont
    , GlyphSize
    , FontName(..)
    , StdFont
    , mkStdFont
) where 


import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import Data.Char 
import qualified Data.Map.Strict as M
import Graphics.PDF.Fonts.Font
import Graphics.PDF.Fonts.AFMParser(getFont)
import System.FilePath 
import Graphics.PDF.Fonts.Encoding
import Graphics.PDF.Fonts.FontTypes


data FontName = Helvetica 
              | Helvetica_Bold
              | Helvetica_Oblique
              | Helvetica_BoldOblique
              | Times_Roman 
              | Times_Bold
              | Times_Italic
              | Times_BoldItalic
              | Courier
              | Courier_Bold
              | Courier_Oblique
              | Courier_BoldOblique
              | Symbol
              | ZapfDingbats
              deriving(Eq,Ord,Enum)


instance Show FontName where
    show Helvetica = "Helvetica"
    show Helvetica_Bold = "Helvetica-Bold"
    show Helvetica_Oblique = "Helvetica-Oblique"
    show Helvetica_BoldOblique = "Helvetica-BoldOblique"
    show Times_Roman = "Times-Roman"
    show Times_Bold = "Times-Bold"
    show Times_Italic = "Times-Italic"
    show Times_BoldItalic = "Times-BoldItalic"
    show Courier = "Courier"
    show Courier_Bold = "Courier-Bold"
    show Courier_Oblique = "Courier-Oblique"
    show Courier_BoldOblique = "Courier-BoldOblique"
    show Symbol = "Symbol"
    show ZapfDingbats = "ZapfDingbats"


instance PdfResourceObject StdFont where
   toRsrc (StdFont f) =  AnyPdfObject . PDFDictionary . M.fromList $
                           [(PDFName "Type",AnyPdfObject . PDFName $ "Font")
                           , (PDFName "Subtype",AnyPdfObject . PDFName $ "Type1")
                           , (PDFName "BaseFont",AnyPdfObject . PDFName $ baseFont f)
                           ] ++ encoding
          where encoding | baseFont f == show Symbol = [] 
                         | baseFont f == show ZapfDingbats = []
                         | otherwise = [(PDFName "Encoding",AnyPdfObject . PDFName $ "MacRomanEncoding")]

instance IsFont StdFont where 
  getDescent (StdFont fs) s = trueSize s $ descent fs 
  getHeight (StdFont fs) s = trueSize s $ height fs 
  getKern (StdFont fs) s a b = trueSize s $ M.findWithDefault 0 (GlyphPair a b) (kernMetrics fs)
  glyphWidth (StdFont fs) s a = trueSize s  $ M.findWithDefault 0 a (widthData fs)
  charGlyph (StdFont fs) c = M.findWithDefault 0 c (encoding fs)
  name (StdFont fs) = baseFont fs 
  hyphenGlyph (StdFont fs) = hyphen fs 
  spaceGlyph (StdFont fs) = space fs

mkStdFont :: FontName -> IO (Maybe AnyFont)
mkStdFont f = do
  let path = "Core14_AFMs" </>  show f <.> "afm" 
  theEncoding <- case f of  
                    ZapfDingbats -> getEncoding ZapfDingbatsEncoding  
                    _ -> getEncoding AdobeStandardEncoding
  theMacEncoding <- case f of 
                     ZapfDingbats -> return Nothing
                     Symbol -> return Nothing 
                     _ -> parseMacEncoding >>= return . Just
  maybeFs <- getFont (Left path) theEncoding theMacEncoding
  case maybeFs of 
    Just theFont -> do
      let f' = theFont { baseFont = show f
                       }
      return . Just . AnyFont . StdFont $ f'
    Nothing -> return Nothing

