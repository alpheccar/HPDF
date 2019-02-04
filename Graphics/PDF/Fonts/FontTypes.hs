{-# LANGUAGE GeneralizedNewtypeDeriving #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Private types for the fonts
---------------------------------------------------------
-- #hide
module Graphics.PDF.Fonts.FontTypes(
	  GlyphSize
    , FontSize 
    , FontStructure(..)
    , GlyphPair(..)
    , FontData(..)
    , StdFont(..)
    , Type1Font(..)
    , mkFlags
	) where

import Graphics.PDF.LowLevel.Types
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.Word 
import Data.Bits hiding(bit)

--Fonts
type FontSize = Int


newtype GlyphSize = GlyphSize Int deriving(Eq,Ord,Num,Integral,Enum,Real)

data GlyphPair = GlyphPair !GlyphCode !GlyphCode deriving(Eq,Ord) 

data FontStructure = FS { baseFont :: String
                        , descent :: !GlyphSize 
                        , ascent :: !GlyphSize
                        , height :: !GlyphSize 
                        , widthData :: M.Map GlyphCode GlyphSize 
                        , kernMetrics :: M.Map GlyphPair GlyphSize 
                        , hyphen :: Maybe GlyphCode 
                        , space :: !GlyphCode
                        , encoding :: M.Map Char GlyphCode
                        , fontBBox :: [PDFFloat]
                        , italicAngle :: !PDFFloat
                        , capHeight :: !GlyphSize 
                        , fixedPitch :: !Bool 
                        , serif :: !Bool 
                        , symbolic :: !Bool 
                        , script :: !Bool 
                        , nonSymbolic :: !Bool 
                        , italic :: !Bool 
                        , allCap :: !Bool 
                        , smallCap :: !Bool 
                        , forceBold :: !Bool 
                        }

mkFlags :: FontStructure -> Word32 
mkFlags fs = bit (fixedPitch fs) 1 .|. 
             bit (serif fs) 2 .|. 
             bit (symbolic fs) 3 .|. 
             bit (script fs) 4 .|. 
             bit (nonSymbolic fs) 6 .|. 
             bit (italic fs) 7 .|. 
             bit (allCap fs) 17 .|. 
             bit (smallCap fs) 18 .|. 
             bit (forceBold fs) 19
    where 
        bit True n = (1 `shiftL` (n-1)) 
        bit False _ = 0

data StdFont = StdFont FontStructure


data Type1Font = Type1Font FontStructure (PDFReference EmbeddedFont)


data FontData = Type1Data B.ByteString

