{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
      IsFont(..)
    , GlyphSize
    , FontName(..)
    , mkStdFont
) where 


import Graphics.PDF.LowLevel.Types
import Graphics.PDF.LowLevel.Kern(kerns)
import Graphics.PDF.Resources
import Data.Char 
import qualified Data.Map as M
import Graphics.PDF.Fonts.Font
import Paths_HPDF

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

instance PdfResourceObject FontName where
   toRsrc f =  AnyPdfObject . PDFDictionary . M.fromList $
                           [(PDFName "Type",AnyPdfObject . PDFName $ "Font")
                           , (PDFName "Subtype",AnyPdfObject . PDFName $ "Type1")
                           , (PDFName "BaseFont",AnyPdfObject . PDFName $ show f)
                           , (PDFName "Encoding",AnyPdfObject . PDFName $ "WinAnsiEncoding")]

foreign import ccall "ctext.h c_getLeading" cgetLeading :: Int -> Int
foreign import ccall "ctext.h c_getAdvance" cgetAdvance :: Int -> Int -> Int
foreign import ccall "ctext.h c_getDescent" cgetDescent :: Int -> Int

_getDescent :: FontName -> FontSize -> PDFFloat
_getDescent n s = trueSize s (cgetDescent (fromEnum n))

_getHeight :: FontName -> FontSize -> PDFFloat
_getHeight n s = trueSize s (cgetLeading (fromEnum n))

-- | Get the kern value for a given font and pair of charcode
_getKern :: (Int,GlyphCode,GlyphCode) -> Int
_getKern (i,GlyphCode a, GlyphCode b) = 
  let k = (i,a,b) 
  in
  M.findWithDefault 0 k kerns

_glyphWidth :: FontName -> FontSize -> GlyphCode -> PDFFloat
_glyphWidth n s c = trueSize s $ cgetAdvance (fromEnum n) (fromEnum c) 
    
_glyphChar :: FontName -> GlyphCode -> Maybe Char 
_glyphChar _ (GlyphCode c) = Just (chr . fromIntegral $ c)

_charGlyph :: f -> Char  -> GlyphCode 
_charGlyph _ = fromIntegral . ord


instance IsFont FontName where 
    getDescent = _getDescent
    getHeight = _getHeight 
    getKern n s a b = trueSize s $ _getKern ((fromEnum n),a,b)
    glyphWidth = _glyphWidth
    glyphChar = _glyphChar
    charGlyph = _charGlyph
    hyphenGlyph f = Just (charGlyph f '-')
    spaceGlyph f = (charGlyph f ' ')
    name f = show f

mkStdFont :: FontName -> AnyFont 
mkStdFont f = AnyFont f

