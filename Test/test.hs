{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
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
            
fontDebug :: PDFFont -> PDFString -> Draw ()
fontDebug f t = do
     drawText $ do
         setFont f
         textStart 10 200.0
         leading $ getHeight f
         renderMode FillText
         displayText t
         startNewLine
         displayText $ toPDFString "Another little test"
     strokeColor $ Rgb 1 0 0
     stroke $ Line 10 200 612 200
     fill $ Circle 10 200 10
     stroke $ Rectangle (10 :+ (200.0 - (getDescent f))) ((10.0 + textWidth f t) :+ (200.0 - getDescent f + getHeight f))

      
geometryTest :: Draw ()
geometryTest  = do
        strokeColor red
        stroke $ Rectangle 0 (200 :+ 100)
        fillColor blue
        fill $ Ellipse 100 100 300 200
        fillAndStroke $ RoundRectangle 32 32 200 200 600 400

lineStyle ::Draw ()
lineStyle  = do
        withNewContext $ do
            setWidth 2
            setDash $ DashPattern [3] 0
            geometryTest
            
            
shadingTest :: Draw ()
shadingTest  = do
     paintWithShading (RadialShading 0 0 50 0 0 600 (Rgb 1 0 0) (Rgb 0 0 1)) (addShape $ Rectangle 0 (300 :+ 300))
     paintWithShading (AxialShading 300 300 600 400 (Rgb 1 0 0) (Rgb 0 0 1)) (addShape $ Ellipse 300 300 600 400)
     
                     
patternTest :: PDFReference PDFPage -> PDF ()
patternTest page = do
     p <- createUncoloredTiling 0 0 100 50 100 50 ConstantSpacing pattern
     cp <- createColoredTiling 0 0 100 50 100 50 ConstantSpacing cpattern
     drawWithPage page $ do
         strokeColor green
         setUncoloredFillPattern p (Rgb 1 0 0)
         fillAndStroke $ Ellipse 0 0 300 300
         setColoredFillPattern cp
         fillAndStroke $ Ellipse 300 300 600 400
         
 where 
       pattern = do
           stroke (Ellipse 0 0 100 50)
       cpattern = do
           strokeColor (Rgb 0 0 1)
           stroke (Ellipse 0 0 100 50) 
           
testAnnotation ::PDFReference PDFPage -> PDF ()
testAnnotation p = do
    drawWithPage p $ do
      r
   
    p1 <- addPage Nothing
    drawWithPage p1 $ do
      withNewContext $ do
        applyMatrix $ translate (50 :+ 0)
        r
    p2 <- addPage Nothing
    drawWithPage p2 $ do
      strokeColor red
      stroke $ Line 0 0 100 0
      applyMatrix $ translate (100 :+ 0)
      strokeColor green
      stroke $ Line 0 0 100 0
      withNewContext $ do
          applyMatrix $ rotate (Degree (-20))
          strokeColor $ Rgb 1 1 0
          stroke $ Line 0 0 100 0
          applyMatrix $ translate (50 :+ 50)
          strokeColor blue
          stroke $ Line 0 0 100 0
          withNewContext $ do
            applyMatrix $ rotate (Degree 45)
            r
            strokeColor black
            stroke $ Line 0 0 100 0
    p3 <- addPage Nothing
    drawWithPage p3 $ do
     withNewContext $ do
        applyMatrix $ scale 3 1
        r
 where r = do
        strokeColor red
        newAnnotation (URLLink (toPDFString "Go to my blog") [0,0,200,100] "http://www.alpheccar.org" True)
        drawText $ text (PDFFont Times_Roman 12) 10 30 (toPDFString "Go to my blog")
        stroke $ Rectangle 0 (200 :+ 100)
        newAnnotation (TextAnnotation (toPDFString "Key annotation") [100,100,130,130] Key)
 
textTest :: Draw ()
textTest = do
    strokeColor red
    fillColor blue
    fontDebug (PDFFont Times_Roman 48) (toPDFString "This is a \\test (éèçàù)!")

              
testImage ::  JpegFile -> PDFReference PDFPage -> PDF ()
testImage jpgf page =  do
    jpg <- createPDFJpeg jpgf
    drawWithPage page $ do
      withNewContext $ do
          setFillAlpha 0.4
          drawXObject jpg
      withNewContext $ do
           applyMatrix $ rotate (Degree 20)
           applyMatrix $ translate (200 :+ 200)
           applyMatrix $ scale 2 2
           drawXObject jpg
      
rawImage ::  PDFReference PDFPage -> PDF ()
rawImage page =  do
    let nb = 200
        getPixel i | i < nb*(3*nb) = 0x00FF0000
                   | i < 2*nb*(3*nb) = 0x0000FF00
                   | otherwise = 0x000000FF
        pixels = U.generate (9*nb*nb) getPixel
    jpg <- createPDFRawImageFromARGB (fromIntegral $ 3*nb) (fromIntegral $ 3*nb) True pixels
    drawWithPage page $ do
      withNewContext $ do
          setFillAlpha 0.4
          applyMatrix $ scale 0.2 0.2
          drawXObject jpg
      withNewContext $ do
           applyMatrix $ rotate (Degree 20)
           applyMatrix $ translate (200 :+ 200)
           applyMatrix $ scale 0.1 0.1
           drawXObject jpg     

data MyParaStyles = Normal
                  | Bold
                  | Crazy
                  | SuperCrazy [Int] [PDFFloat]
                  | DebugStyle
                  | RedRectStyle
                  | BlueStyle
                  
instance ComparableStyle MyParaStyles where
  isSameStyleAs Normal Normal = True
  isSameStyleAs Bold Bold = True
  isSameStyleAs Crazy Crazy = True
  isSameStyleAs (SuperCrazy _ _) (SuperCrazy _ _) = True
  isSameStyleAs DebugStyle DebugStyle = True
  isSameStyleAs RedRectStyle RedRectStyle = True
  isSameStyleAs BlueStyle BlueStyle = True
  isSameStyleAs _ _ = False
  
                  
instance Style MyParaStyles where
    textStyle Normal = TextStyle (PDFFont Times_Roman 10) black black FillText 1.0 1.0 1.0 1.0
    textStyle Bold = TextStyle (PDFFont Times_Bold 12) black black FillText 1.0 1.0 1.0 1.0
    textStyle RedRectStyle = TextStyle (PDFFont Times_Roman 10) black black FillText 1.0 1.0 1.0 1.0
    textStyle DebugStyle = TextStyle (PDFFont Times_Roman 10) black black FillText 1.0 1.0 1.0 1.0
    textStyle Crazy = TextStyle (PDFFont Times_Roman 10) red red FillText 1.0 1.0 1.0 1.0
    textStyle (SuperCrazy _ _) = TextStyle (PDFFont Times_Roman 12) black black FillText 1.0 2.0 0.5 0.5
    textStyle BlueStyle = TextStyle (PDFFont Times_Roman 10) black black FillText 1.0 1.0 1.0 1.0
    
    sentenceStyle BlueStyle = Just $ \r d -> do
        fillColor $ Rgb 0.6 0.6 1
        strokeColor $ Rgb 0.6 0.6 1
        fillAndStroke r
        d
        return()
    
    sentenceStyle RedRectStyle = Just $ \r d -> do
        strokeColor red
        stroke r
        d
        return()
    sentenceStyle Crazy = Just $ \r d -> do
       d
       strokeColor blue
       stroke r
    sentenceStyle _ = Nothing
           
    wordStyle DebugStyle = Just $ \r m d ->
      case m of
          DrawWord -> d >> return ()
          DrawGlue -> d >> stroke r
    wordStyle Crazy = Just crazyWord
    wordStyle (SuperCrazy l _) = Just ws 
     where
        ws _ DrawGlue _ = return ()
        ws (Rectangle (xa :+ ya) (xb :+ yb)) DrawWord drawWord = do
            let [a,b,c,d,e,f,g,h] :: [PDFFloat] = map (\x -> x / 16.0) . map fromIntegral . take 8 $ l
                --angle = head angl
                p = Polygon [ (xa-a) :+ (ya+b)
                            , (xb+c) :+ (ya+d)
                            , (xb+e) :+ (yb-f)
                            , (xa-g) :+ (yb-h)
                            , (xa-a) :+ (ya+b)
                            ]
            strokeColor red
            stroke p
            fillColor $ Rgb 0.8 1.0 0.8
            fill p
            withNewContext $ do
              --applyMatrix . rotate . Degree $ angle
              drawWord
            return ()

    wordStyle _ = Nothing
    
    updateStyle (SuperCrazy a b) = SuperCrazy (drop 8 a) (tail b)
    updateStyle a = a
    
    styleHeight r@(SuperCrazy _ _) = (getHeight . textFont . textStyle $ r) + 4.0
    styleHeight r = getHeight . textFont . textStyle $ r
    
    styleDescent r@(SuperCrazy _ _) = (getDescent . textFont . textStyle $ r) + 2
    styleDescent r = getDescent . textFont . textStyle $ r
    

             
crazyWord :: Rectangle -> StyleFunction -> Draw a -> Draw ()
crazyWord r@(Rectangle (xa :+ ya) (xb :+ yb)) DrawWord d = do
    fillColor $ Rgb 0.6 1 0.6 
    fill r
    d
    strokeColor $ Rgb 0 0 1
    let m = (ya+yb)/2.0
    stroke $ Line xa m xb m 
crazyWord (Rectangle (xa :+ ya) (xb :+ yb)) DrawGlue _ = do
    fillColor $ Rgb 0 0 1
    fill (Circle ((xa+xb)/2.0) ((ya+yb)/2.0) ((xb-xa)/2.0))
    
   
    
superCrazy :: MyParaStyles
superCrazy = SuperCrazy (randomRs (0,32) (mkStdGen 0)) (randomRs (-10.0,10.0) (mkStdGen 10000))
    
data MyVertStyles = NormalPara
                  | CirclePara
                  | BluePara !PDFFloat

instance ComparableStyle MyVertStyles where
    isSameStyleAs NormalPara NormalPara = True
    isSameStyleAs CirclePara CirclePara = True
    isSameStyleAs (BluePara _) (BluePara _) = True
    isSameStyleAs _ _ = False
    
    
instance ParagraphStyle MyVertStyles MyParaStyles  where
    lineWidth (BluePara a) w nb = (if nb > 3 then w else w-a) - 20.0
    lineWidth CirclePara _ nb = 
           let nbLines = 15.0
               r = nbLines * (getHeight . textFont . textStyle $ Normal)
               pasin x' = if x' >= 1.0 then pi/2 else if x' <= -1.0 then (-pi/2) else asin x'
               angle l = pasin $ (nbLines - (fromIntegral l)  ) / nbLines
           in
           abs(2*r*cos (angle nb))
    lineWidth _ w _ = w
           
    linePosition (BluePara a) _ nb = (if nb > 3 then 0.0 else a) + 10.0
    linePosition a@(CirclePara) w nb = max 0 ((w - lineWidth a w nb) / 2.0)
    linePosition _ _ _ = 0.0
    
    interline (BluePara _) = Just $ \r -> do
        fillColor $ Rgb 0.6 0.6 1
        strokeColor $ Rgb 0.6 0.6 1
        fillAndStroke r
    interline _ = Nothing
        
    paragraphChange (BluePara _) _ (AChar st c _:l) = 
        let f = PDFFont Helvetica_Bold 45
            w' = charWidth f c 
            charRect = Rectangle (0 :+ (- getDescent f)) (w' :+ (getHeight f - getDescent f))
            c' = mkLetter (0,0,0) Nothing . mkDrawBox $ do
                withNewContext $ do
                    applyMatrix $ translate ((-w') :+ (getDescent f - getHeight f + styleHeight st - styleDescent st))
                    fillColor $ Rgb 0.6 0.6 1
                    strokeColor $ Rgb 0.6 0.6 1
                    fillAndStroke $ charRect
                    fillColor black
                    drawText $ do
                        renderMode AddToClip
                        textStart 0 0
                        setFont f
                        displayText (toPDFString [c])
                    paintWithShading (AxialShading 0 (- getDescent f) w' (getHeight f - getDescent f) (Rgb 1 0 0) (Rgb 0 0 1)) (addShape charRect)
        in
        (BluePara w', c':l)
    
    paragraphChange s _ l = (s,l)
    
    paragraphStyle (BluePara _) = Just $ \(Rectangle (xa :+ ya) (xb :+ yb)) b -> do
        let f = Rectangle ((xa-3) :+ (ya-3)) ((xb+3) :+ (yb+3))
        fillColor $ Rgb 0.6 0.6 1
        fill f
        b
        strokeColor red
        stroke f
        return ()
    paragraphStyle _ = Nothing
    
            
containerTest :: PDFReference PDFPage -> Rectangle ->( TM StandardParagraphStyle StandardStyle ())   -> PDF ()
containerTest p (Rectangle (xa :+ ya) (xb :+ yb)) theText = 
    let c = mkContainer xa ya xb yb 0
        (d,c',r) = fillContainer (defaultVerState NormalParagraph) c . getBoxes NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
            theText
        cb = mkContainer xa (ya-100) xb yb 0
        (d',c'',_) = fillContainer (defaultVerState NormalParagraph) cb r
    in drawWithPage p $ do
      d
      d'
      let x = containerX c'
          y = containerY c'
          w = containerWidth c'
          --h = containerHeight c'
      strokeColor red
      stroke $ Rectangle (x :+ y) ((x+w) :+ (ya-100-yb))
      strokeColor blue
      stroke $ containerContentRectangle c'
      stroke $ containerContentRectangle c''
      
    
standardStyleTest :: TM StandardParagraphStyle StandardStyle ()
standardStyleTest = do
    paragraph $ do
        txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut "
        txt $ "labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris "
        setStyle $ Font (PDFFont Times_Bold 10) black black
        txt $ "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate "
        txt $ "velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non "
        txt $ "proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
 
testText :: TM StandardParagraphStyle StandardStyle ()      
testText = do
        paragraph $ do
            --txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
            txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
            txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
            txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
            txt $ "irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
            txt $ "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia "
            txt $ "deserunt mollit anim id est laborum."
            
testBreakText :: TM StandardParagraphStyle StandardStyle ()      
testBreakText = do
        paragraph $ do
            txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
            txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
            txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
            txt $ "irure dolor"
            forceNewLine
            txt $ " in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
            txt $ "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia "
            txt $ "deserunt mollit anim id est laborum."
                    
typesetTest :: Int -> PDFReference PDFPage -> PDF ()
typesetTest test page = do
    let simpleText = do
            paragraph $ do
                txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
                txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
                txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
                txt $ "irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
                txt $ "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia "
                txt $ "deserunt mollit anim id est laborum."
        debugText = do
            paragraph $ do
                txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor"
                setStyle Bold
                txt $ " incididunt ut labore et dolore magna aliqua. "
                setStyle Normal
                txt $ "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure "
                setStyle RedRectStyle
                txt $ "dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. "
                setStyle Normal
            glue 3 0 0
            paragraph $ do
                txt $ "Excepteur sint occaecat cupidatat non"
                txt $ " proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                setStyle superCrazy
                txt $ " And now, a super crazy style to test the code. "
                setStyle Normal
                txt $ "Return to a normal style :-)"
            glue 3 0 0
            paragraph $ do
                txt $ "More crazy styles ... "
                setStyle Crazy
                par
                setStyle Normal
        par = do
              txt $ "Despite the trip to a rather unseemly part of "
              txt $ "Paris, it was well worth it. I bumped into Guido, Parisian "
              txt $ "boutique L'Eclaireur's most stylish salesperson, whose "
              txt $ "presence confirmed the ultra hip factor of the crowd in "
              txt $ "attendance. The performances were strong and varied, "
              txt $ "and the experience of tapping into the Burlesque "
              txt $ "renaissance /--/-- mixed in with a little business of "
              txt $ "fashion /--/-- was a great lesson learned on how to throw "
              txt $ "an authentic fashion event. "
              
        normalPar :: Para MyParaStyles ()
        normalPar = do
              txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
              txt $ "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor "
              txt $ "in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, "
              txt $ "sunt in culpa qui officia deserunt mollit anim id est laborum."
        myText = do
              -- Duplicate paragraph several times
              paragraph normalPar
              glue 6 0.33 0
              setStyle BlueStyle
              setParaStyle (BluePara 0)
              setFirstPassTolerance 500
              --setSecondPassTolerance 10000
              unstyledGlue 6 0.33 0
              paragraph $ do
                    txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
                    txt $ "Ut enim ad minim veniam, quis nostrud exercitation ullamco"
                    --addBox symbol 10 10 5
                    txt $ " laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor "
                    txt $ "in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, "
                    txt $ "sunt in culpa qui officia deserunt mollit anim id est laborum."
              unstyledGlue 6 0.33 0
              setFirstPassTolerance 100
              setSecondPassTolerance 200
              setStyle Normal
              setParaStyle NormalPara
              glue 6 0.33 0
              paragraph normalPar
              glue 6 0.33 0
              paragraph normalPar
        --textStart = 300 - getHeight f + getDescent f
        maxw = 400
    drawWithPage page $ do
      --strokeColor red
      --setWidth 0.5
      --stroke $ Rectangle 10 0 (10+maxw) 300
      --stroke $ Line 10 textStart (10+maxw) textStart
      strokeColor black
      case test of
          0 -> do
                strokeColor red
                stroke $ Line 10 300 (10+100) 300
                displayFormattedText (Rectangle (10 :+ 0) ((10+100) :+ 300)) NormalPara Normal simpleText
                
          1 -> do
              strokeColor red
              stroke $ Line 10 400 (10+maxw) 400
              displayFormattedText (Rectangle (10 :+ 0) ((10+maxw) :+ 400)) NormalPara Normal myText
          2 -> do
              strokeColor red
              stroke $ Line 10 300 (10+maxw) 300
              displayFormattedText (Rectangle (10 :+ 0) ((10+maxw) :+ 300)) NormalPara Normal debugText
          3 -> do
              let r = (Rectangle (10 :+ 200) ((10+maxw) :+ 300))
              displayFormattedText r CirclePara Normal $ do
                   setStyle Normal
                   setFirstPassTolerance 5000
                   setSecondPassTolerance 5000
                   --setLineSkip 0 0 0
                   --setBaseLineSkip 0 0 0
                   --setLineSkipLimit 0
                   paragraph $ do
                       mapM_ (const normalPar) ([1..3]::[Int])
                       txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
                       txt $ "Ut enim ad minim"
              strokeColor red
              stroke r
          4 -> displayFormattedText (Rectangle (10 :+ 0) ((10+maxw) :+ 300)) NormalParagraph (Font (PDFFont Times_Roman 10) black black) standardStyleTest
          5 -> do
              strokeColor red
              stroke $ Rectangle (10 :+ 300) ((10+maxw) :+ 400)
              displayFormattedText (Rectangle (10 :+ 300) ((10+maxw) :+ 400)) NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
                  setJustification Centered
                  testText
                  
              strokeColor red
              stroke $ Rectangle (10 :+ 200) ((10+maxw) :+ 300)
              displayFormattedText (Rectangle (10 :+ 200) ((10+maxw) :+ 300)) NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
                  setJustification LeftJustification
                  testText
              
              strokeColor red
              stroke $ Rectangle (10 :+ 100) ((10+maxw) :+ 200)
              displayFormattedText (Rectangle (10 :+ 100) ((10+maxw) :+ 200)) NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
                  setJustification RightJustification
                  testText
              strokeColor red
              fillColor blue
              stroke $ Rectangle (10 :+ 0) ((10+maxw) :+ 100)
              drawText $ text (PDFFont Helvetica_Bold 24) 10 100 (toPDFString "Lorem ipsum")
              stroke $ Line 10 120 (10 + textWidth (PDFFont Helvetica_Bold 24) (toPDFString "Lorem ipsum") ) 120 
              displayFormattedText (Rectangle (10 :+ 0) ((10+maxw) :+ 100)) NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
                  setJustification LeftJustification
                  paragraph $ do
                      setStyle (Font (PDFFont Helvetica_Bold 24) black black)
                      txt $ "Lorem ipsum"
          6 -> do
                strokeColor red
                stroke $ Rectangle (10 :+ 300) ((10+maxw) :+ 400)
                displayFormattedText (Rectangle (10 :+ 300) ((10+maxw) :+ 400)) NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
                    setJustification Centered
                    testBreakText

                strokeColor red
                stroke $ Rectangle (10 :+ 200) ((10+maxw) :+ 300)
                displayFormattedText (Rectangle (10 :+ 200) ((10+maxw) :+ 300)) NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
                    setJustification LeftJustification
                    testBreakText
                
                strokeColor red
                stroke $ Rectangle (10 :+ 100) ((10+maxw) :+ 200)
                displayFormattedText (Rectangle (10 :+ 100) ((10+maxw) :+ 200)) NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
                    setJustification RightJustification
                    testBreakText
                    
                strokeColor red
                stroke $ Rectangle (10 :+ 0) ((10+maxw) :+ 100)
                displayFormattedText (Rectangle (10 :+ 0) ((10+maxw) :+ 100)) NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
                    setJustification FullJustification
                    testBreakText
          _ -> displayFormattedText (Rectangle (0 :+ 300) ((10+maxw) :+ 300)) NormalPara Normal myText
     
textBoxes :: Draw ()
textBoxes = do
    let x = 230
        y = 200
        w = 220
        h = 200
    fillColor blue
    fill $ Circle x y 5
    let (r,b) = drawTextBox x y w h S NormalParagraph (Font (PDFFont Times_Roman 10) black black) $ do
                setJustification FullJustification
                paragraph $ do
                    txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
                    txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
                    txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
                    txt $ "irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
                    txt $ "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia "
                    txt $ "deserunt mollit anim id est laborum."
    b
    strokeColor red
    stroke r
    return ()
                                       
testAll :: JpegFile -> PDF ()
testAll jpg = do
    page1 <- addPage Nothing
    newSection (toPDFString "Typesetting") Nothing Nothing $ do
     newSection (toPDFString "Normal text") Nothing Nothing $ do
        typesetTest 1 page1
     
     page2 <- addPage Nothing
     newSection (toPDFString "Debug text") Nothing Nothing $ do
            typesetTest 2 page2
          
     page3 <- addPage Nothing
     newSection (toPDFString "Circle text") Nothing Nothing $ do
            typesetTest 3 page3
          
     page3a <- addPage Nothing
     newSection (toPDFString "Standard styles") Nothing Nothing $ do
            typesetTest 4 page3a
            
     page3b <- addPage Nothing
     newSection (toPDFString "Justifications") Nothing Nothing $ do
            typesetTest 5 page3b
            
     page3d <- addPage Nothing
     newSection (toPDFString "New lines") Nothing Nothing $ do
            typesetTest 6 page3d
          
     page3c <- addPage Nothing
     newSection (toPDFString "Container") Nothing Nothing $ do
            containerTest page3c (Rectangle (10 :+ 300)  (100 :+ 100)) testText
            containerTest page3c (Rectangle (210 :+ 300) (200 :+ 100)) $ do
                setJustification Centered
                testText
        
    page4 <- addPage Nothing
    newSection (toPDFString "Shapes") Nothing Nothing $ do
        
      newSection (toPDFString "Geometry") Nothing Nothing $ do
         drawWithPage page4 $ do
           geometryTest 
           
      page5 <- addPage Nothing
      newSection (toPDFString "Line style") Nothing Nothing $ do
          drawWithPage page5 $ do
            lineStyle
            
      page6 <- addPage Nothing
      newSection (toPDFString "Object reuse") Nothing Nothing $ do
           r <- createPDFXForm 0 0 200 200 lineStyle
           drawWithPage page6 $ do
                drawXObject r
            
    page7 <- addPage Nothing
    newSectionWithPage (toPDFString "Painting") Nothing Nothing page7 $ do
     newSection (toPDFString "Patterns") Nothing Nothing $ do
        patternTest page7
        
     page8 <- addPage Nothing
     newSection (toPDFString "Shading") Nothing Nothing $ do
        drawWithPage page8 $ do
          shadingTest
          
    page9 <- addPage Nothing
    newSection (toPDFString "Media") Nothing Nothing $ do
      newSection (toPDFString "image") Nothing Nothing $ do   
           testImage jpg page9  
           
    page10 <- addPage Nothing
    newSection (toPDFString "Annotations") Nothing Nothing $ do
          testAnnotation page10
          
    page11 <- addPage Nothing
    newSection (toPDFString "Text encoding") Nothing Nothing $ do
      drawWithPage page11 $ do
        textTest
    newSection (toPDFString "Fun") Nothing Nothing $ do
        penrose
    page12 <- addPage Nothing
    newSection (toPDFString "Text box") Nothing Nothing $ do
      drawWithPage page12 $ do
        textBoxes
    page13 <- addPage Nothing
    rawImage page13
    
        
main :: IO()
main = do
    let rect = PDFRect 0 0 600 400
    Right jpg <- readJpegFile "logo.jpg"  
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
        testAll jpg
    --print $ charWidth (PDFFont Times_Roman 1) '('
     