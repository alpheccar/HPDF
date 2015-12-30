{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Box
---------------------------------------------------------
-- #hide
module Graphics.PDF.Typesetting.Layout (

   Container(..)
 , Width
 , Height
 , VBox(..)
 , ParagraphStyle(..)
 , VerState(..)
 , vglue
 , addTo
 , isOverfull
 , mkContainer
 , strokeVBoxes
 , containerX
 , containerY
 , containerWidth
 , containerHeight
 , containerContentHeight
 , containerContentRightBorder
 , containerContentLeftBorder
 , containerCurrentHeight
 , containerContentRectangle
 , containerParaTolerance
 ) where

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Typesetting.Breaking
import Graphics.PDF.Draw
import Graphics.PDF.Coordinates
import Graphics.PDF.Shapes(Rectangle(..))
import Graphics.PDF.Typesetting.Box
import Data.List(foldl')
import Data.Maybe(isJust,fromJust)


data VerState s = VerState { baselineskip :: !(PDFFloat,PDFFloat,PDFFloat) -- ^ Default value (12,0.17,0.0)
                           , lineskip :: !(PDFFloat,PDFFloat,PDFFloat) -- ^ Default value (3.0,0.33,0.0)
                           , lineskiplimit :: !PDFFloat -- ^ Default value 2
                           , currentParagraphStyle :: !s
                           }
                           
data VBox ps s = Paragraph Int [Letter s] !(Maybe ps) !BRState
               | VBox !PDFFloat !PDFFloat !PDFFloat ![VBox ps s] !(Maybe ps)
               | VGlue !PDFFloat !PDFFloat !PDFFloat !(Maybe (PDFFloat,PDFFloat)) !(Maybe ps)
               | SomeVBox !PDFFloat !BoxDimension !AnyBox !(Maybe ps)

notGlue :: VBox ps s -> Bool
notGlue (VGlue _ _ _ _ _) = False
notGlue (Paragraph _ _ _ _) = False
notGlue _ = True
                              
vglue :: Maybe ps
     -> PDFFloat -- ^ Glue height
     -> PDFFloat -- ^ Glue dilatation factor
     -> PDFFloat -- ^ Glue compression factor
     -> PDFFloat -- ^ Glue width
     -> PDFFloat -- ^ Glue delta
     -> VBox ps s
vglue s h y z width delta = VGlue h width delta (Just(y,z)) s

instance Show (VBox ps s) where
 show (VBox _ a _ l _) = "(VBox " ++ show a ++ " " ++ show l ++ ")"
 show (VGlue a _ _ _ _) = "(VGlue " ++ show a ++ ")"
 show (Paragraph _ _ _ _) = "(Paragraph)"
 show (SomeVBox _ d t _) = "(SomeVBox " ++ show (boxHeight d) ++ " " ++ show t ++ ")"

instance MaybeGlue (VBox ps s) where
  glueSizeWithRatio (VGlue w _ _ (Just(y,z)) _) r = glueSize w y z r
  glueSizeWithRatio a _ = boxHeight a
  
  glueY (VGlue _ _ _ (Just(y,_)) _)  = y
  glueY _ = 0
  glueZ (VGlue _ _ _ (Just(_,z)) _)  = z
  glueZ _ = 0

instance Box (VBox ps s) where
   boxWidth (Paragraph _ _ _ _) = 0
   boxWidth (VBox w _ _ _ _) = w
   boxWidth (SomeVBox _ d _ _)  = boxWidth d
   boxWidth (VGlue _ w _ _ _)  = w

   boxHeight (Paragraph _ _ _ _) = 0
   boxHeight (VBox _ h _ _ _) = h
   boxHeight (SomeVBox _ d _ _) = boxHeight d
   boxHeight (VGlue h _ _ _ _) = h

   boxDescent (Paragraph _ _ _ _) = 0
   boxDescent (VBox _ _ d _ _) = d
   boxDescent (SomeVBox _ d _ _) = boxDescent d
   boxDescent (VGlue _ _ _ _ _) = 0

instance (ParagraphStyle ps s) => DisplayableBox (VBox ps s) where
        strokeBox (Paragraph _ _ _ _) _ _ = return ()
        strokeBox b@(VBox _ _ _ l _) x y'' = strokeVBoxes l x y'
          where
              y' = y'' - boxHeight b
        strokeBox (VGlue h w delta _ (Just style)) x y = 
            if (isJust . interline $ style)
                then
                    (fromJust . interline $ style) $ Rectangle ((x+delta) :+ (y-h)) ((x+w+delta) :+ y)
                else
                   return()
        strokeBox (VGlue _ _ _ _ _) _ _ = return ()

        strokeBox (SomeVBox delta _ a _) x y = strokeBox a (x+delta) y
                
type Width = PDFFloat
type Height = PDFFloat

-- | Container for vboxes (x,y,width,maxheight,height,currenty,current z, tolerance para)
-- tolerance para means a paragraph is not started if too close from the bottom edge of the box
data Container ps s = Container PDFFloat PDFFloat Width PDFFloat PDFFloat PDFFloat PDFFloat PDFFloat [VBox ps s]

-- | Create a empty container to constraint the amount of line that can be displayed
mkContainer :: PDFFloat -- ^ x
            -> PDFFloat -- ^ y
            -> PDFFloat -- ^ width
            -> PDFFloat -- ^ height
            -> PDFFloat -- ^ Pargraph tolerance
            -> Container ps s -- ^ New container
mkContainer x y width height tol = Container x y width height 0 0 0 tol []            

-- | Get the width of the container
containerWidth :: Container ps s -> PDFFloat
containerWidth (Container _ _ w _ _ _ _ _ _) = w

-- | Get the width of the container
containerParaTolerance :: Container ps s -> PDFFloat
containerParaTolerance (Container _ _ _ _ _ _ _ t _) = t

-- | Get the height of the container
containerHeight :: Container ps s -> PDFFloat
containerHeight (Container _ _ _ h _ _ _ _ _) = h

-- | Get the current height of the container without glue dilatation
containerCurrentHeight :: Container ps s -> PDFFloat
containerCurrentHeight (Container _ _ _ _ ch _ _ _ _) = ch

-- | Get the content height of the container with glue dilatation
containerContentHeight :: Container ps s -> PDFFloat
containerContentHeight (Container _ _ _ maxh h y z _ _) = let r = min (dilatationRatio maxh h y z) 2.0 in
 glueSize h y z r
 
-- | Get the minimum left border of the container content
containerContentLeftBorder :: Container ps s -> PDFFloat
containerContentLeftBorder (Container _ _ _ _ _ _ _ _ []) = 0.0
containerContentLeftBorder (Container _ _ _ _ _ _ _ _ l) = minimum . map getBoxDelta $ l
   
-- | Get the maximum right border of the container content (maybe bigger than container width due to overfull lines)
containerContentRightBorder :: Container ps s -> PDFFloat
containerContentRightBorder (Container _ _ _ _ _ _ _ _ []) = 0.0
containerContentRightBorder (Container _ _ _ _ _ _ _ _ l) = 
 let xmax = maximum . map rightBorder $ l
     rightBorder x = getBoxDelta x + boxWidth x
 in
  xmax

-- | Container horizontal position
containerX :: Container ps s -> PDFFloat
containerX (Container x _ _ _ _ _ _ _ _) = x

-- | Container vertical position
containerY :: Container ps s -> PDFFloat
containerY (Container _ y _ _ _ _ _ _ _) = y

-- | Return the rectangle containing the text after formatting and glue dilatation
containerContentRectangle :: Container ps s -> Rectangle
containerContentRectangle c = Rectangle ((x+l) :+ (y-th)) ((x+r) :+ y)
 where
    x = containerX c
    y = containerY c
    th = containerContentHeight c
    l = containerContentLeftBorder c
    r = containerContentRightBorder c


-- | Get the required style for the interline glue
getInterlineStyle :: ComparableStyle ps => VBox ps s -> VBox ps s -> Maybe ps
getInterlineStyle (VBox _ _ _ _ (Just s)) (SomeVBox _ _ _ (Just s')) | s `isSameStyleAs` s' = Just s
                                                                     | otherwise = Nothing

getInterlineStyle (VBox _ _ _ _ (Just s)) (VBox _ _ _ _ (Just s')) |  s `isSameStyleAs`  s' = Just s
                                                                   | otherwise = Nothing

getInterlineStyle (SomeVBox _ _ _ (Just s)) (SomeVBox _ _ _ (Just s')) |  s `isSameStyleAs`  s' = Just s
                                                                       | otherwise = Nothing

getInterlineStyle (SomeVBox _ _ _ (Just s)) (VBox _ _ _ _ (Just s')) |  s `isSameStyleAs`  s' = Just s
                                                                     | otherwise = Nothing

getInterlineStyle _ _ = Nothing

-- | Interline glue required
interlineGlue :: ComparableStyle ps => VerState ps -> VBox ps s -> VBox ps s -> Maybe (VBox ps s, PDFFloat, PDFFloat)
interlineGlue settings a b | notGlue a && notGlue b = 
    let p = boxDescent a
        h = boxHeight b - boxDescent b
        (ba,by,bz) = baselineskip settings
        (lw,ly,lz) = lineskip settings
        li = lineskiplimit settings
        istyle = getInterlineStyle a b
        theWidth = boxWidth a
        theDelta = getBoxDelta a
    in
    if p <= -1000 
        then
            Nothing
        else
            if ba - p - h >= li
                then
                    Just $ (vglue istyle (ba-p-h) by bz theWidth theDelta,by,bz)
                else
                    Just $ (vglue istyle lw ly lz theWidth theDelta,ly,lz)
                                  | otherwise = Nothing

addTo :: ComparableStyle ps => VerState ps -> VBox ps s -> Container ps s -> Container ps s
addTo _ line (Container px py w maxh h y z t []) = Container px py w maxh ((boxHeight line)+h) y z t [line]
addTo settings line (Container px py w maxh h y z t l@(a:_)) = 
    case interlineGlue settings a line of
        Nothing ->
          let h' = boxHeight line + h 
              y' = y + glueY line
              z' = z + glueZ line
          in
          Container px py w maxh h' y' z' t (line:l)
        Just (v,ny,nz) ->
          let h' = boxHeight line + h + boxHeight v
              y' = y + ny + glueY line
              z' = z + nz + glueZ line
          in
          Container px py w maxh h' y' z' t (line:v:l)
          
isOverfull :: Container ps s -> Bool
isOverfull (Container _ _ _ maxh h y z _ _) = let r = dilatationRatio maxh h y z
 in
   if r >= bigAdjustRatio then h > maxh else r <= -1



-- | Paragraph style
class (ComparableStyle a, Style s) => ParagraphStyle a s | a -> s where
    -- | Width of the line of the paragraph
    lineWidth :: a -- ^ The style
               -> PDFFloat -- ^ Width of the text area used by the typesetting algorithm
               -> Int -- ^ Line number
               -> PDFFloat -- ^ Line width
    
    -- | Horizontal shift of the line position relatively to the left egde of the paragraph bounding box
    linePosition :: a -- ^ The style 
                  -> PDFFloat -- ^ Width of the text area used by the typesetting algorithm
                  -> Int -- ^ Line number 
                  -> PDFFloat -- ^ Horizontal offset from the left edge of the text area
    
    -- | How to style the interline glues added in a paragraph by the line breaking algorithm
    interline :: a -- ^ The style 
              -> Maybe (Rectangle -> Draw ()) -- ^ Function used to style interline glues
    interline _ = Nothing
    lineWidth _ w _ = w
    linePosition _ _ = const 0.0
    
    -- | Change the content of a paragraph before the line breaking algorithm is run. It may also change the style
    paragraphChange :: a -- ^ The style
               -> Int -- ^ Line offset different from 0 when a paragraph has been broken
               -> [Letter s] -- ^ List of letters in the paragraph
               -> (a,[Letter s]) -- ^ Update style and list of letters
    paragraphChange a _ l = (a,l)
    
    -- | Get the paragraph bounding box and the paragraph draw command to apply additional effects
    paragraphStyle :: a -- ^ The style 
                   -> Maybe (Rectangle -> Draw b -> Draw ()) -- ^ Function used to style a paragraph
    paragraphStyle _ = Nothing

-- | Get the delta used to position a box with non rectangular shapes
getBoxDelta :: VBox ps s -> PDFFloat
getBoxDelta (Paragraph _ _ _ _) = 0.0
getBoxDelta (VBox _ _ _ _ _) = 0.0
getBoxDelta (VGlue _ _ delta _ _) = delta
getBoxDelta (SomeVBox delta _ _ _) = delta




isSameParaStyle :: ComparableStyle ps => ps -> VBox ps s -> Bool
isSameParaStyle s (Paragraph _ _ (Just s') _) =  s `isSameStyleAs`  s'
isSameParaStyle s (VBox _ _ _ _ (Just s')) =  s `isSameStyleAs`  s'
isSameParaStyle s (VGlue _ _ _ _ (Just s')) =  s `isSameStyleAs`  s'
isSameParaStyle s (SomeVBox _ _ _ (Just s'))  =  s `isSameStyleAs`  s'     
isSameParaStyle _ _ = False

recurseStrokeVBoxes :: (ParagraphStyle ps s) => Int -> [VBox ps s] -> PDFFloat -> PDFFloat -> Draw ()
recurseStrokeVBoxes _ [] _ _  = return ()
recurseStrokeVBoxes _ (Paragraph _ _ _ _:_) _ _ = return ()
recurseStrokeVBoxes nb (a@(VGlue _ _ _ _ _):l) xa y  = do
    let h = boxHeight a
    strokeBox a xa y
    recurseStrokeVBoxes nb l xa (y-h)

recurseStrokeVBoxes nb (a:l) xa y = do
    let h = boxHeight a
    strokeBox a xa y
    recurseStrokeVBoxes (nb+1) l xa (y-h)

drawWithParaStyle :: (ParagraphStyle ps s) => ps -> [VBox ps s] -> PDFFloat -> PDFFloat -> Draw ()   
drawWithParaStyle style b xa y' = do
    let  (l',l'') = span (isSameParaStyle style) b
         h' = foldl' (\x' ny -> x' + boxHeight ny) 0.0 l'
    if (isJust . paragraphStyle $ style)
      then do
          let xleft = (minimum $ 100000:map getBoxDelta l' ) + xa
              xright = (maximum $ 0:(map (\x -> boxWidth x + getBoxDelta x) l')) + xa
          (fromJust . paragraphStyle $ style) (Rectangle (xleft :+ (y'- h')) (xright :+ y')) (recurseStrokeVBoxes 1 l' xa y')
      else
         recurseStrokeVBoxes 1 l' xa y'
    strokeVBoxes l'' xa (y' - h')

-- | Stroke the VBoxes
strokeVBoxes :: (ParagraphStyle ps s) => [VBox ps s] -- ^ List of boxes
             -> PDFFloat -- ^ X pos
             -> PDFFloat -- ^ Y pos
             -> Draw ()
strokeVBoxes [] _ _ = return ()
strokeVBoxes b@((Paragraph _ _ (Just s') _):_) xa y = drawWithParaStyle s' b xa y 
strokeVBoxes b@((VBox _ _ _ _ (Just s')):_) xa y = drawWithParaStyle s' b xa y 
strokeVBoxes b@((VGlue _ _ _ _ (Just s')):_) xa y = drawWithParaStyle s' b xa y
strokeVBoxes b@((SomeVBox _ _ _ (Just s')):_) xa y = drawWithParaStyle s' b xa y
strokeVBoxes (a:l) xa y = 
    do
        let h = boxHeight a
        strokeBox a xa y
        strokeVBoxes l xa (y-h)
        