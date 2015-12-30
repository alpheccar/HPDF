---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Horizontal mode
---------------------------------------------------------
-- #hide
{-# LANGUAGE CPP #-}
module Graphics.PDF.Typesetting.Horizontal (
   HBox(..)
 , mkHboxWithRatio
 , horizontalPostProcess
 ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Typesetting.Breaking
import Graphics.PDF.Shapes
import Graphics.PDF.Draw
import Graphics.PDF.Coordinates
import qualified Data.ByteString.Char8 as S(pack)
import Data.Maybe(isJust,fromJust)
import Data.List(foldl')
import Graphics.PDF.Colors
import Graphics.PDF.Text
import Graphics.PDF.Typesetting.Box
import Control.Monad.Writer(tell)
import Control.Monad(when)
import Graphics.PDF.LowLevel.Serializer

-- | Current word (created from letter) is converted to a PDFString
saveCurrentword :: String -> PDFString
saveCurrentword = PDFString . S.pack . reverse

-- WARNING
-- According to splitText, PDFText to concatenate ARE letters so we can optimize the code
-- Sentences are created when no word style is present, otherwise we just create words
createWords :: ComparableStyle s => PDFFloat -- ^ Adjustement ratio
            -> Maybe (s,String, PDFFloat) -- ^ Current word
            -> [Letter s] -- ^ List of letters
            -> [HBox s] -- ^ List of words or sentences

createWords _ Nothing [] = []
-- Empty list, current word or sentence is added
createWords _ (Just (s,t,w)) [] = [createText s (saveCurrentword t) w]

-- Start of a new word
createWords r Nothing ((AChar s t w):l) = createWords r (Just (s,[t],w)) l
-- New letter. Same style added to the word. Otherwise we start a new word
createWords r (Just (s,t,w)) ((AChar s' t' w'):l) | s `isSameStyleAs` s' = createWords r (Just (s,t':t,w+w')) l
                                                  | otherwise = (createText s (saveCurrentword $ t) w):createWords r (Just (s',[t'],w')) l 
                                                             
-- Glue close the word and start a new one because we want glues of different widths in the PDF
createWords r (Just (s,t,w)) ((Glue w' y z (Just s')):l) = (createText s (saveCurrentword $ t) w):(HGlue w' (Just(y,z)) (Just s')):createWords r  Nothing l

-- Penalties are invisible. The are needed just to compute breaks
createWords r c (Penalty _:l) = createWords r  c l
createWords r c (FlaggedPenalty _ _ _:l) = createWords r  c l

-- We just add the box
createWords r Nothing ((Glue w' y z s):l) = (HGlue w' (Just(y,z)) s):createWords r Nothing l
createWords r (Just (s,t,w)) ((Glue w' y z Nothing):l) = (createText s (saveCurrentword $ t) w):(HGlue w' (Just(y,z)) Nothing):createWords r Nothing l
  
createWords r Nothing ((Kern w' s):l) = (HGlue w' Nothing s):createWords r Nothing l
createWords r (Just (s,t,w)) ((Kern w' s'):l) = (createText s (saveCurrentword $ t) w):(HGlue w' Nothing s'):createWords r Nothing l

createWords r Nothing ((Letter d a s):l) = (SomeHBox d a s):createWords r Nothing l
createWords r (Just (s,t,w)) ((Letter d a st):l) = (createText s (saveCurrentword $ t) w):(SomeHBox d a st):createWords r Nothing l
 

-- | horizontalPostProcess
horizontalPostProcess :: (Style s) => [(PDFFloat,[Letter s],[Letter s])] -- ^ adjust ratio, hyphen style, list of letters or boxes
                      -> [(HBox s,[Letter s])] -- ^ List of lines
horizontalPostProcess [] = []
horizontalPostProcess ((r,l',r'):l) = let l'' = createWords r Nothing . simplify $ l' in
  if null l''
      then
          horizontalPostProcess l
      else
          ((mkHboxWithRatio r l''),r'):horizontalPostProcess l 


-- | An horizontal Hbox (sentence or word)
-- The width of the glue was computed with the adjustement ratio of the HLine containing the glue
-- The width of the text is already taking into account the adjustement ratio of the HLine containing the Text
-- Otherwise, HBox cannot dilate or compress. 
data HBox s = HBox !PDFFloat !PDFFloat !PDFFloat ![HBox s]
            | HGlue !PDFFloat !(Maybe (PDFFloat,PDFFloat)) !(Maybe s)
            | Text !s !PDFString !PDFFloat
            | SomeHBox !BoxDimension !AnyBox !(Maybe s)
     
-- | Change the style of the box      
withNewStyle :: s -> HBox s -> HBox s
withNewStyle _ a@(HBox _ _ _ _) = a
withNewStyle s (HGlue a b _) = HGlue a b (Just s)
withNewStyle s (Text _ a b) = Text s a b
withNewStyle s (SomeHBox d a _) = SomeHBox d a (Just s) 
    
-- | A line of hboxes with an adjustement ratio required to display the text (generate the PDF command to increase space size)       
--data HLine = HLine !PDFFloat ![HBox] deriving(Show)

mkHboxWithRatio :: Style s => PDFFloat -- ^ Adjustement ratio
                -> [HBox s]
                -> HBox s
mkHboxWithRatio _ [] = error "Cannot create an empty horizontal box"
mkHboxWithRatio r l = 
    let w = foldl' (\x y -> x + glueSizeWithRatio y r) 0.0 l
        --h = maximum . map boxHeight $ l
        ascent = maximum . map boxAscent $ l
        d = maximum . map boxDescent $ l
        h = ascent + d
        addBox (HGlue gw (Just(y,z)) s) (HBox w' h' d' l') = HBox w' h' d' (HGlue (glueSize gw y z r) Nothing s:l')
        addBox a (HBox w' h' d' l') = HBox w' h' d' (a:l')
        addBox _ _ = error "We can add boxes only to an horizontal list"
    in
    -- Add boxes and dilate glues when needing fixing their dimensions after dilatation
    foldr addBox (HBox w h d []) l
    
instance Style s => MaybeGlue (HBox s) where
    glueSizeWithRatio (HGlue w (Just(y,z)) _) r = glueSize w y z r
    glueSizeWithRatio a _ = boxWidth a
    glueY (HGlue _ (Just(y,_)) _)  = y
    glueY _ = 0
    glueZ (HGlue _ (Just(_,z)) _)  = z
    glueZ _ = 0
    
-- | Create an HBox           
createText :: s -- ^ Style
           -> PDFString -- ^ String
           -> PDFFloat -- ^ Width
           -> HBox s
createText s t w = Text s t w


instance Show (HBox s) where
   show (HBox _ _ _ a) = "(HBox " ++ show a ++ ")"
   show (HGlue a _ _) = "(HGlue " ++ show a ++ ")"
   show (Text _ t _) = "(Text " ++ show t ++ ")"
   show (SomeHBox _ t _) = "(SomeHBox " ++ show t ++ ")"

-- | Draw a line of words and glue using the word style
drawTextLine :: (Style s) => s -> [HBox s] -> PDFFloat -> PDFFloat -> Draw ()
drawTextLine  _ [] _ _ = return ()
drawTextLine  style l@(a:l') x y | (isJust . wordStyle $ style) =  do
    let  h = boxHeight a
         d = boxDescent a
         y' = y + h - d
    strokeBox (withNewStyle style a) x y'
    drawTextLine (updateStyle style) l' (x + boxWidth a) y
                                 | otherwise = drawWords style l x y
    
-- | Draw a line of words, glue, or any box without word style
drawWords :: (Style s) => s -> [HBox s] -> PDFFloat -> PDFFloat -> Draw ()
drawWords _ [] _ _ = return ()

drawWords s ((Text _ t w):l) x y = do
    (l',x') <- drawText $ do
       drawTheTextBox StartText s x y (Just t)
       drawPureWords s l (x + w) y
    drawWords s l' x' y
    
drawWords s l@((HGlue _ _ _ ):_) x y = do
    (l',x') <- drawText $ do
       drawTheTextBox StartText s x y Nothing
       drawPureWords s l x y
    drawWords s l' x' y
    
drawWords s (a@(SomeHBox _ _ _):l) x y =  do
    let h = boxHeight a
        d = boxDescent a
        w = boxWidth a
        y' = y - d + h
    strokeBox a x y'
    drawWords s l (x + w) y

drawWords _ _ _ _ = return ()

-- | Draw only words and glues using PDF text commands
drawPureWords :: Style s => s -> [HBox s] -> PDFFloat -> PDFFloat -> PDFText ([HBox s],PDFFloat)  

drawPureWords s [] x y = do
    drawTheTextBox StopText s x y Nothing
    return ([],x)
          
drawPureWords s ((Text _ t w):l) x y = do
    drawTheTextBox ContinueText s x y (Just t)
    drawPureWords s l (x + w) y
    
drawPureWords s ((HGlue w _ _):l) x y = do
    drawTextGlue s w
    drawPureWords s l (x + w) y  
    
drawPureWords s l@((SomeHBox _ _ _):_) x y = do
    drawTheTextBox StopText s x y Nothing
    return (l,x)
    
drawPureWords s (_:l) x y = drawPureWords s l x y  
 
-- When a start of line is detected by drawLineOfHBoxes, we start the drawing
startDrawingNewLineOfText :: (Style s) => PDFFloat -> PDFFloat -> [HBox s] -> PDFFloat -> PDFFloat -> s -> Draw ()
startDrawingNewLineOfText hl dl l x y style = 
    do
           -- Position of draw line based upon the whole line and not just this word
       let y' = y - hl + dl
           (l',l'') = span (isSameStyle style) l
           w' = foldl' (\x' ny -> x' + boxWidth ny) 0.0 l'
       if (isJust . sentenceStyle $ style)
             then do
                 (fromJust . sentenceStyle $ style) (Rectangle (x :+ (y - hl)) ((x+w') :+ y)) (drawTextLine style l' x y')
             else do
                 drawTextLine style l' x y'
       drawLineOfHboxes hl dl l'' (x + w') y
    

drawLineOfHboxes :: (Style s) => PDFFloat -- ^ Height of the total line first time this function is called
                 -> PDFFloat -- ^ Descent of the total line first time this function is called
                 -> [HBox s] -- ^ Remaining box to display
                 -> PDFFloat -- ^ x for the remaining boxes
                 -> PDFFloat -- ^ y for the whole line
                 -> Draw ()
drawLineOfHboxes _ _ [] _ _ = return ()
-- | Start a new text
drawLineOfHboxes hl dl l@((Text style _ _):_) x y = startDrawingNewLineOfText hl dl l x y style
drawLineOfHboxes hl dl l@((HGlue _ _ (Just style)):_) x y = startDrawingNewLineOfText hl dl l x y style

drawLineOfHboxes hl dl (a:l) x y = do
      let  h = boxHeight a
           d = boxDescent a
           -- Compute top of box a
           y' = y  - hl + dl - d + h
      strokeBox a x y'
      drawLineOfHboxes hl dl l (x + boxWidth a) y

instance Style s => Box (HBox s) where
     boxWidth (Text _ _ w) = w
     boxWidth (HBox w _ _ _) = w
     boxWidth (SomeHBox d _ _)  = boxWidth d
     boxWidth (HGlue w _ _)  = w 
                  
     boxHeight (Text style _ _) = styleHeight style
     boxHeight (HBox _ h _ _) = h
     boxHeight (SomeHBox d _ _) = boxHeight d
     boxHeight (HGlue _ _ (Just s)) = styleHeight s
     boxHeight (HGlue _ _ _) = 0
    
     boxDescent (Text style _ _) = styleDescent style
     boxDescent (HBox _ _ d _) = d
     boxDescent (SomeHBox d _ _) = boxDescent d
     boxDescent (HGlue _ _ (Just s)) = styleDescent s
     boxDescent (HGlue _ _ _) = 0
     
               
-- Draw a text box
drawTheTextBox :: Style style => TextDrawingState
               -> style
               -> PDFFloat
               -> PDFFloat
               -> Maybe PDFString
               -> PDFText ()
drawTheTextBox state style x y t = do
  when (state == StartText || state == OneBlock) $ (do
     setFont (textFont . textStyle $ style)
     strokeColor (textStrokeColor . textStyle $ style)
     fillColor (textFillColor . textStyle $ style)
     renderMode (textMode . textStyle $ style)
     setWidth (penWidth . textStyle $ style)
     textStart x y
     tell $ mconcat [newline,lbracket])
  -- Here we need to dilate the space to take into account r and the font setting
  when (state == StartText || state == OneBlock || state == ContinueText) $ (do
      case t of
          Nothing -> return ()
          Just myText -> tell $ toPDF myText
    )
  when (state == StopText || state == OneBlock) $ (do
      tell rbracket
      tell $ serialize " TJ")
      
-- | Draw the additional displacement required for a space in a text due to the dilaton of the glue
drawTextGlue :: Style style => style
             -> PDFFloat
             -> PDFText ()
drawTextGlue style w = do              
    let ws = (textWidth (textFont . textStyle $ style) (toPDFString " "))
        PDFFont _ size = textFont . textStyle $ style
        delta = w - ws 
    return ()
    tell . mconcat $ [ lparen, bspace,rparen,bspace,toPDF ((-delta) * 1000.0 / (fromIntegral size) ), bspace]  
    
  
data TextDrawingState = StartText -- ^ Send PDF commands needed to start a text
                      | ContinueText -- ^ Continue adding text
                      | StopText -- ^ Stop the text
                      | OneBlock -- ^ One block of text
                      deriving(Eq)
              
instance (Style s) => DisplayableBox (HBox s) where
     strokeBox a@(HBox _ _ _ l) x y = do
         let he = boxHeight a
             de = boxDescent a
         drawLineOfHboxes he de l x y
        
     strokeBox a@(HGlue w _ (Just style)) x y = do
         let de = boxDescent a
             he = boxHeight a
             y' = y - he + de
         -- In word mode we have to apply a special function to the word
         -- otherwise we apply a different function to the sentence
         if (isJust . wordStyle $ style)
             then
                 (fromJust . wordStyle $ style) (Rectangle (x :+ (y' - de)) ((x+w) :+ (y' - de + he))) DrawGlue (return ())
             else
                 return ()
                 
     strokeBox a@(Text style t w) x y = do
         let de = boxDescent a
             he = boxHeight a
             y' = y - he + de
         -- In word mode we have to apply a special function to the word
         -- otherwise we apply a different function to the sentence
         if (isJust . wordStyle $ style)
             then
                 (fromJust . wordStyle $ style) (Rectangle (x :+ (y' - de)) ((x+w) :+ (y' - de + he))) DrawWord (drawText $ drawTheTextBox OneBlock style x y' (Just t))
             else 
                 drawText $ drawTheTextBox OneBlock style x y' (Just t)

     strokeBox (SomeHBox _ a _) x y = strokeBox a x y
     strokeBox (HGlue _ _ _) _ _ = return ()
     
 -- Test is a box has same style
isSameStyle :: (Style s) => s 
         -> HBox s
         -> Bool
isSameStyle s (Text style _ _) = s `isSameStyleAs` style
isSameStyle s (HGlue _ _ (Just style)) = s `isSameStyleAs` style
isSameStyle s (SomeHBox _ _ (Just style)) = s `isSameStyleAs` style
isSameStyle _ _ = False
