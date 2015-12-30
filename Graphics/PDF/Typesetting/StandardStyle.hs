{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Standard styles for typesettings
---------------------------------------------------------
-- #hide
module Graphics.PDF.Typesetting.StandardStyle(
 -- * Styles
   StandardStyle(..)
 , StandardParagraphStyle(..)
 ) where
     
import Graphics.PDF.Colors
import Graphics.PDF.Text
import Graphics.PDF.Typesetting.Vertical
import Graphics.PDF.Typesetting.Box
     
-- | Standard styles for sentences
data StandardStyle = Font PDFFont Color Color

-- | Standard styles for paragraphs
data StandardParagraphStyle = NormalParagraph


instance ComparableStyle StandardStyle where
  isSameStyleAs (Font a sca fca) (Font b scb fcb) = a == b && sca == scb && fca == fcb
  --isSameStyleAs _ _ = False
    
instance ComparableStyle StandardParagraphStyle where
  isSameStyleAs NormalParagraph NormalParagraph = True 
  
instance Style StandardStyle where
    textStyle (Font a sc fc) = TextStyle a sc fc FillText 1.0 1.0 1.0 1.0    

instance ParagraphStyle StandardParagraphStyle StandardStyle       