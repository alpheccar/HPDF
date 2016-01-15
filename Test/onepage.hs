{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2013, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Test
---------------------------------------------------------


module Main where


import Graphics.PDF
import System.Random
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import Network.URI 
import Data.Maybe(fromJust)
import Data.Maybe(fromJust)
import Graphics.PDF.Fonts.Font
import Graphics.PDF.Fonts.StandardFont

alpheccarURL = fromJust $ parseURI "http://www.alpheccar.org"

vertical = 200.0
margin = 10.0 
debugText = "é"
debugFontSize = 12
lightBlue= Rgb 0.6 0.6 1.0

data MyParaStyles = DebugStyle AnyFont
data MyVertStyles = NormalPara
                  

instance ComparableStyle MyParaStyles where
  isSameStyleAs (DebugStyle fa) (DebugStyle fb) = fa == fb

instance Style MyParaStyles where
    textStyle (DebugStyle f) = TextStyle (PDFFont f debugFontSize) black black FillText 1.0 1.0 1.0 1.0
    
    sentenceStyle _ = Nothing
           
    wordStyle (DebugStyle _) = Just $ \r m d ->
      case m of
          DrawWord -> d >> setWidth 0.5 >> strokeColor red >> stroke r
          DrawGlue -> d >> setWidth 0.5 >> fillColor lightBlue >> fill r
    
    updateStyle a = a
    
instance ComparableStyle MyVertStyles where
    isSameStyleAs NormalPara NormalPara = True

instance ParagraphStyle MyVertStyles MyParaStyles  where
        

testAll ::  PDFFont  -> PDF ()
testAll theFont@(PDFFont f s) = do
    page1 <- addPage Nothing
    drawWithPage page1 $ do 
      displayFormattedText (Rectangle (10 :+ 0) ((10+100) :+ 300)) (NormalPara) (DebugStyle f)  $
        paragraph $ do
          txt $ debugText
      strokeColor black
      drawText $ do
          setFont theFont
          textStart margin vertical
          displayText debugText
      setWidth 0.5
      stroke $ Rectangle (margin :+ (vertical - (getDescent f s))) ((margin + textWidth theFont debugText) :+ (vertical - getDescent f s + getHeight f s))

        
main :: IO()
main = do
    Just timesRoman <- mkStdFont Times_Roman 
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author= "alpheccar éèçàü", compressed = False}) rect $ do
        testAll (PDFFont timesRoman debugFontSize)
     