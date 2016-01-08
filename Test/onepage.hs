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
import Penrose
import System.Random
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import Network.URI 
import Data.Maybe(fromJust)

alpheccarURL = fromJust $ parseURI "http://www.alpheccar.org"


                                       
testAll :: JpegFile -> PDF ()
testAll jpg = do
    page1 <- addPage Nothing
    drawWithPage page1 $ do 
           strokeColor red
           --newAnnotation (URLLink  ("Go to my blog") [0,0,200,100] alpheccarURL True)
           drawText $ text (PDFFont Times_Roman 12) 10 30 ("Go to my blog")
           stroke $ Rectangle 0 (200 :+ 100)
           --newAnnotation (TextAnnotation ("Key annotation éàü") [100,100,130,130] Key)
        
main :: IO()
main = do
    let rect = PDFRect 0 0 600 400
    Right jpg <- readJpegFile "logo.jpg"  
    runPdf "demo.pdf" (standardDocInfo { author= "alpheccar éèçàü", compressed = False}) rect $ do
        testAll jpg
    --print $ charWidth (PDFFont Times_Roman 1) '('
     