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
    , FontStructure(..)
    , GlyphPair(..)
    , emptyFontStructure
    , fontSize
    , trueSize
) where 

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import qualified Data.Map.Strict as M

-- Fonts
type FontSize = Int


newtype GlyphSize = GlyphSize Int deriving(Eq,Ord,Num,Integral,Enum,Real)

data GlyphPair = GlyphPair !GlyphCode !GlyphCode deriving(Eq,Ord) 

data FontStructure = FS { baseFont :: String
                        , descent :: !GlyphSize 
                        , height :: !GlyphSize 
                        , width :: M.Map GlyphCode GlyphSize 
                        , kern :: M.Map GlyphPair GlyphSize 
                        , hyphen :: Maybe GlyphCode 
                        , space :: !GlyphCode
                        , encoding :: M.Map Char GlyphCode
                        }

emptyFontStructure :: FontStructure
emptyFontStructure = FS "" 0 0 M.empty M.empty Nothing 0 M.empty

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

