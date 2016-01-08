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
module Graphics.PDF.Font(
      IsFont(..)
    , GlyphSize
) where 

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.LowLevel.Kern(kerns)
import Graphics.PDF.Resources
import Data.Char 
import qualified Data.Map as M

foreign import ccall "ctext.h c_getLeading" cgetLeading :: Int -> Int
foreign import ccall "ctext.h c_getAdvance" cgetAdvance :: Int -> Int -> Int
foreign import ccall "ctext.h c_getDescent" cgetDescent :: Int -> Int
foreign import ccall "ctext.h c_hasKern" hasKern :: Int -> Bool



-- Only useful for standard PDF fonts
c2i :: Char -> Int 
c2i = ord

-- Only useful for standard PDF fonts
g2c :: PDFFont -> GlyphCode -> Char 
g2c _ (GlyphCode c) = chr . fromIntegral $ c

newtype GlyphSize = GlyphSize Int deriving(Eq,Ord,Num,Integral,Enum,Real)


class IsFont f where
    {-
    Font descriptions
    -}
    getDescent :: f -> PDFFloat
    getHeight :: f  -> PDFFloat
    fontSize :: f -> FontSize
    deviceUnit :: f -> GlyphSize -> PDFFloat
    {-
    Font metrics
    -}
    getKern :: f -> GlyphCode -> GlyphCode -> PDFFloat
    glyphWidth :: f -> GlyphCode -> PDFFloat
    {-
    Font convertions
    -}
    hyphenGlyph :: f -> Maybe GlyphCode
    spaceGlyph :: f -> GlyphCode
    glyphChar :: f -> GlyphCode -> Maybe Char
    charGlyph :: f -> Char  -> GlyphCode 

{-
    
Standard PDF Fonts

-}
-- pixel size / 2048 gives factor 

trueSize :: Int -> Int -> PDFFloat
trueSize fontSize textSize = (fromIntegral (textSize*fontSize)) / 1000.0

_getDescent :: PDFFont -> PDFFloat
_getDescent (PDFFont n s) = trueSize s (cgetDescent (fromEnum n))

_getHeight :: PDFFont -> PDFFloat
_getHeight (PDFFont n s) = trueSize s (cgetLeading (fromEnum n))

-- | Get the kern value for a given font and pair of charcode
_getKern :: (Int,GlyphCode,GlyphCode) -> Int
_getKern (i,GlyphCode a, GlyphCode b) = 
  let k = (i,a,b) 
  in
  M.findWithDefault 0 k kerns

_glyphWidth :: PDFFont -> GlyphCode -> PDFFloat
_glyphWidth (PDFFont n s) c = 
    let w = cgetAdvance (fromEnum n) (fromEnum c) 
    in
    trueSize s w

_glyphChar :: PDFFont -> GlyphCode -> Maybe Char 
_glyphChar _ (GlyphCode c) = Just (chr . fromIntegral $ c)

_charGlyph :: f -> Char  -> GlyphCode 
_charGlyph _ = fromIntegral . ord


instance IsFont PDFFont where 
    getDescent = _getDescent
    getHeight = _getHeight 
    getKern (PDFFont n s) a b = trueSize s $ _getKern ((fromEnum n),a,b)
    deviceUnit (PDFFont _ s) g = trueSize s (fromIntegral g)
    fontSize (PDFFont _ s) = s
    glyphWidth = _glyphWidth
    glyphChar = _glyphChar
    charGlyph = _charGlyph
    hyphenGlyph f = Just (charGlyph f '-')
    spaceGlyph f = (charGlyph f ' ')
