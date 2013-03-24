module Penrose (
 penrose
 )where
    
import Graphics.PDF

golden :: PDFFloat
golden = ((sqrt 5) + 1) / 2

phi :: PDFFloat
phi = 36 / 180 * pi

width :: PDFFloat
width = 300

myBlue :: Color
myBlue = Rgb 0.8 0.8 1

myGreen :: Color
myGreen = Rgb 0.8 1 0.8

data Tile = A | B  | A' | B' 

tilea :: PDFFloat -> Tile -> Int -> Draw ()
tilea angle k n = withNewContext $ do
    applyMatrix (translate (width :+ 0))
    applyMatrix (rotate . Degree $ angle)
    applyMatrix (scale (1/golden) (1/golden))
    divide (n-1) k 


tileb :: PDFFloat -> Tile -> Int ->  Draw ()
tileb angle k n = withNewContext $ do
    applyMatrix (translate ((width*golden) :+ 0))
    applyMatrix (rotate . Degree $ angle)
    applyMatrix (scale (1/golden) (1/golden))
    divide (n-1) k


divide :: Int -> Tile -> Draw ()
divide n A | n == 0 = a width 
           | otherwise = do
               tilea 108 A n
               tilea 180 B' n

divide n A' | n == 0 = a' width
            | otherwise = do
                tilea (-108) A' n
                tilea 180 B n

divide n B | n == 0  = b width
           | otherwise = do
               tileb 144 B n
               tilea 108 A n
               tilea 180 B' n

divide n B' | n == 0  = b' width
            | otherwise = do
               tileb (-144) B' n 
               tilea (-108) A' n
               tilea 180 B n


b :: PDFFloat  -> Draw () 
b s = do
      setFillAlpha 0.8
      fillColor myBlue
      strokeColor myBlue
      let pol = [ 0
                , mkPolar s phi 
                , ((s*golden) :+ 0)
                ]
      fillAndStroke (Polygon pol)
      strokeColor black 
      stroke (Polygon pol)
     

b' :: PDFFloat  -> Draw () 
b' s = withNewContext $ do 
          applyMatrix (scale 1 (-1))
          b s


a :: PDFFloat  -> Draw () 
a s = do
      setFillAlpha 0.8
      fillColor myGreen
      strokeColor myGreen
      let pol = [ 0
                , mkPolar s phi
                , (s :+ 0)
                ]     
      fillAndStroke (Polygon pol)
      strokeColor black 
      stroke (Polygon pol)

a' :: PDFFloat  -> Draw () 
a' s = withNewContext $ do
           applyMatrix (scale 1 (-1))
           a s

penrose :: PDF ()  
penrose  =  do
    page <- addPage (Just (PDFRect 0 0 (round (1.5*width)) (round width)))
    newSection (toPDFString "Penrose") Nothing Nothing $ do
        drawWithPage page $ do
            applyMatrix (translate (20 :+ 5))
            applyMatrix (rotate . Degree $ 36) 
            let r = 4
            divide r B 
            divide r B'


           