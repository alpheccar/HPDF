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
-- PDF Font
---------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Graphics.PDF.Fonts.Font(
      IsFont(..)
    , GlyphSize
    , FontSize 
    , PDFFont(..)
    , AnyFont(..)
    , FontStructure
    , EmbeddedFont
    , FontData
    , emptyFontStructure
    , fontSize
    , trueSize
    , readFontData
) where 

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Graphics.PDF.Fonts.FontTypes

emptyFontStructure :: FontStructure
emptyFontStructure = FS { baseFont = ""
                        , descent = 0
                        , ascent  = 0
                        , height = 0
                        , widthData = M.empty
                        , kernMetrics = M.empty
                        , hyphen = Nothing
                        , space = 0
                        , encoding = M.empty
                        , fontBBox = []
                        , italicAngle = 0
                        , capHeight = 0
                        , fixedPitch = False
                        , serif = False
                        , symbolic = False
                        , script = False
                        , nonSymbolic = False
                        , italic = False
                        , allCap = False
                        , smallCap = False
                        , forceBold = False
                        }

class IsFont f where
    {-
    Font descriptions
    -}
    name :: f -> String
    {-
    Font metrics
    -}
    getDescent :: f -> FontSize -> PDFFloat
    getHeight :: f -> FontSize -> PDFFloat
    {-
    Glyph metrics
    -}
    getKern :: f -> FontSize -> GlyphCode -> GlyphCode  -> PDFFloat
    glyphWidth :: f -> FontSize -> GlyphCode  -> PDFFloat
    {-
    Font convertions
    -}
    hyphenGlyph :: f -> Maybe GlyphCode
    spaceGlyph :: f -> GlyphCode
    charGlyph :: f -> Char  -> GlyphCode 

data AnyFont = forall f. (IsFont f,PdfResourceObject f) => AnyFont f

instance PdfResourceObject AnyFont where
   toRsrc (AnyFont f) = toRsrc f

instance IsFont AnyFont where 
    name (AnyFont f) = name f 
    getDescent (AnyFont f) = getDescent f
    getHeight (AnyFont f) = getHeight f
    {-
    Font metrics
    -}
    getKern (AnyFont f) = getKern f
    glyphWidth (AnyFont f) = glyphWidth f
    {-
    Font convertions
    -}
    hyphenGlyph (AnyFont f) = hyphenGlyph f
    spaceGlyph (AnyFont f) = spaceGlyph f
    charGlyph (AnyFont f) = charGlyph f

instance Eq AnyFont where 
    a == b = name a == name b

instance Ord AnyFont where 
    compare a b = compare (name a) (name b)

data PDFFont = PDFFont AnyFont FontSize deriving(Eq)

fontSize :: PDFFont -> FontSize 
fontSize (PDFFont _ s) = s

instance Ord PDFFont where
    compare (PDFFont na sa) (PDFFont nb sb) = if sa == sb then compare na nb else compare sa sb

-- pixel size / 2048 gives factor 

trueSize :: Int -> GlyphSize -> PDFFloat
trueSize fs glyphSize = (fromIntegral glyphSize * fromIntegral fs) / 1000.0





readFontData :: FilePath -> IO FontData 
readFontData f = do 
    r <- B.readFile f 
    return (Type1Data r)



