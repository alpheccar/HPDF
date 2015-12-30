{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Graphics.PDF.Typesetting.Box (
    Box(..)
  , DisplayableBox(..)
  , AnyBox(..)
  , Style(..)
  , TextStyle(..)
  , StyleFunction(..)
  , BoxDimension
  , DrawBox
  , ComparableStyle(..)
  , mkDrawBox
 ) where
     
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Text
import Graphics.PDF.Shapes
import Graphics.PDF.Coordinates

-- | Make a drawing box. A box object containing a Draw value
mkDrawBox :: Draw () -> DrawBox
mkDrawBox d = DrawBox d

-- | A box containing a Draw value
newtype DrawBox = DrawBox (Draw())

instance Box DrawBox where
    boxWidth _ = 0
    boxHeight _ = 0
    boxDescent _ = 0
    
instance DisplayableBox DrawBox where
    strokeBox (DrawBox a) x y = do
        withNewContext $ do
            applyMatrix $ translate (x :+ y)
            a
    
instance Show DrawBox where
    show _ = "DrawBox"

-- | Dimension of a box : width, height and descent
type BoxDimension = (PDFFloat,PDFFloat,PDFFloat)

-- | Text style used by PDF operators
data TextStyle = TextStyle { textFont :: !PDFFont
                           , textStrokeColor :: !Color
                           , textFillColor :: !Color
                           , textMode :: !TextMode
                           , penWidth :: !PDFFloat
                           , scaleSpace :: !PDFFloat -- ^ Scaling factor for normal space size (scale also the dilation and compression factors)
                           , scaleDilatation :: !PDFFloat -- ^ Scale the dilation factor of glues
                           , scaleCompression :: !PDFFloat -- ^ Scale the compression factor of glues
                           }
                           deriving(Eq)
             
-- | What kind of style drawing function is required for a word
-- when word styling is enabled              
data StyleFunction = DrawWord -- ^ Must style a word
                   | DrawGlue -- ^ Must style a glue
                   deriving(Eq)  
                   
-- | Used to compare two style without taking into account the style state
class ComparableStyle a where   
    isSameStyleAs :: a -> a -> Bool                     
                  
-- | Style of text  (sentences and words). Minimum definition textStyle      
class ComparableStyle a => Style a where
    -- ^ Modify the look of a sentence (sequence of words using the same style on a line)
    sentenceStyle :: a -- ^ The style
                  -> Maybe (Rectangle -> Draw b -> Draw ()) -- ^ Function receiving the bounding rectangle and the command for drawing the sentence
    sentenceStyle _ = Nothing
    -- ^ Modify the look of a word
    wordStyle :: a -- ^ The style
              -> Maybe (Rectangle -> StyleFunction -> Draw b -> Draw ()) -- ^ Word styling function
    wordStyle _ = Nothing
    textStyle :: a -> TextStyle
    -- | A style may contain data changed from word to word
    updateStyle :: a -> a
    updateStyle = id
    
    -- | A style may change the height of words
    -- 
    -- > Default implementation
    -- > styleHeight = getHeight . textFont . textStyle
    -- 
    styleHeight :: a -> PDFFloat
    
    -- | A style may change the descent of lines
    --
    -- > Default implementation
    -- > styleDescent = getDescent . textFont . textStyle
    --
    styleDescent :: a -> PDFFloat
    styleHeight = getHeight . textFont . textStyle 
    styleDescent = getDescent . textFont . textStyle 

-- | A box is an object with dimensions and used in the typesetting process
class Box a where
     -- | Box width
     boxWidth :: a -- ^ Box
              -> PDFFloat -- ^ Width of the box
              
     -- | Box height
     boxHeight :: a -> PDFFloat
     -- | Distance between box bottom and box baseline
     boxDescent :: a -> PDFFloat
     -- | Distance between box top and box baseline
     boxAscent :: a -> PDFFloat
     boxAscent a = boxHeight a - boxDescent a
     
instance Box BoxDimension where
    boxWidth (w,_,_) = w
    boxHeight (_,h,_) = h
    boxDescent (_,_,d) = d

-- | A box that can be displayed
class DisplayableBox a where
     -- | Draw a box
     strokeBox :: a -- ^ The box
               -> PDFFloat -- ^ Horizontal position
               -> PDFFloat -- ^ Vertical position (top of the box and NOT baseline)
               -> Draw ()
    
instance Box AnyBox where
    boxWidth (AnyBox a)  = boxWidth a
    boxHeight (AnyBox a) = boxHeight a
    boxDescent (AnyBox a) = boxDescent a

instance DisplayableBox AnyBox where
    strokeBox (AnyBox a)  = strokeBox a
  
instance Show AnyBox where
    show (AnyBox a)  = show a
    
data AnyBox = forall a. (Show a,Box a, DisplayableBox a) => AnyBox a
