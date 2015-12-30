---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Vertical mode
---------------------------------------------------------
-- #hide
module Graphics.PDF.Typesetting.Vertical (
    mkVboxWithRatio
  , vglue
  , defaultVerState
  , ParagraphStyle(..)
  , VerState(..)
  , fillContainer
  , mkContainer
  , VBox(..)
 ) where
 
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Typesetting.Breaking
import Graphics.PDF.Typesetting.Horizontal(horizontalPostProcess,HBox)
import Graphics.PDF.Draw
import Graphics.PDF.Typesetting.Box
import Data.List(foldl')
import Graphics.PDF.Typesetting.Layout
        
-- | Default vertical state
--
-- > Default values
-- > baselineskip = (12,0.17,0.0)
-- > lineskip = (3.0,0.33,0.0)
-- > lineskiplimit = 2
--
defaultVerState :: s -> VerState s                
defaultVerState s = VerState { baselineskip = (12,0.17,0.0)
                             , lineskip = (3.0,0.33,0.0)
                             , lineskiplimit = 2
                             , currentParagraphStyle = s
                             }

-- | Pair of functions describing the shape of a text areas : horizontal position of each line, vertical top of the area, width of each line
-- First line is 1


           

                         
-- | A line of hboxes with an adjustement ratio required to display the text (generate the PDF command to increase space size)       
--data HLine = HLine !PDFFloat ![HBox] deriving(Show)

mkVboxWithRatio :: PDFFloat -- ^ Adjustement ratio
                -> [VBox ps s]
                -> VBox ps s
mkVboxWithRatio _ [] = error "Cannot make an empty vbox"
mkVboxWithRatio r l = 
   let w = foldl' (\x y -> x + glueSizeWithRatio y r) 0.0 l
       h = maximum . map boxHeight $ l
       d = maximum . map boxDescent $ l
       addBox (VGlue gw gh gdelta (Just(y,z)) s) (VBox w' h' d' l' s') = VBox w' h' d' (VGlue (glueSize gw y z r) gh gdelta Nothing s:l') s'
       addBox a (VBox w' h' d' l' s') = VBox w' h' d' (a:l') s'
       addBox _ _ = error "We can add boxes only to an horizontal list"
   in
   -- Add boxes and dilate glues when needing fixing their dimensions after dilatation
   foldr addBox (VBox w h d [] Nothing) l







    
dilateVboxes :: PDFFloat -> VBox ps s -> VBox ps s
dilateVboxes r g@(VGlue _ w l (Just(_,_)) s) = 
    let h' = glueSizeWithRatio g r
    in
      VGlue h' w l Nothing s
dilateVboxes _ g@(VGlue _ _ _ Nothing _) = g
dilateVboxes _ a = a              

drawContainer :: ParagraphStyle ps s => Container ps s -- ^ Container
              -> Draw ()
drawContainer (Container px py _ maxh h y z _ oldl) = 
    let l' = reverse oldl
        r = min (dilatationRatio maxh h y z) 2.0
        l'' = map (dilateVboxes r) l'
    in
      strokeVBoxes l'' px py
      
-- | Create a new paragraph from the remaining letters
createPara :: Int
           -> Maybe ps
           -> BRState
           -> [Letter s] 
           -> [VBox ps s]
createPara _ _ _ [] = []
createPara lineOffset style paraSettings l = [Paragraph lineOffset (simplify l) style paraSettings]

-- | Add paragraph lines to a container
addParaLine :: (ParagraphStyle ps s, ComparableStyle ps) => VerState ps
            -> Maybe ps
            -> BRState 
            -> Container ps s -- ^ Container
            -> [((HBox s,[Letter s]),Int)]
            -> Either (Draw (),Container ps s,[VBox ps s]) (Container ps s)
addParaLine _ _ _ c  [] = Right c
addParaLine verstate style paraSettings c (((line,remainingPar),lineNb):l) = 
    let c' = addTo verstate (toVBoxes style (containerWidth c) line lineNb) c 
    in
    if isOverfull c'
        then
         Left (drawContainer c,c,createPara lineNb style paraSettings remainingPar)
        else
         addParaLine verstate style paraSettings c' l

-- | Fill a container with lines
fillContainer :: (ParagraphStyle ps s, ComparableStyle ps) => VerState ps -- ^ Vertical style for interline glues
              -> Container ps s -- ^ Container
              -> [VBox ps s] -- ^ VBox to add
              -> (Draw(),Container ps s,[VBox ps s]) -- ^ Component to draw, new container and remaining VBoxes due to overfull container
fillContainer _ c [] = (drawContainer c,c,[])
fillContainer verstate c para@(Paragraph lineOffset l style paraSettings:l') = 
    if containerContentHeight c > containerHeight c - containerParaTolerance c
    then
        (drawContainer c,c,para)
    else
        let (fl,newStyle) = case style of
                    Nothing -> (formatList paraSettings (const $ containerWidth c) l,Nothing) 
                    Just aStyle -> let (style',nl) = paragraphChange aStyle lineOffset l 
                                   in
                                   (formatList paraSettings (\nb -> (lineWidth style') (containerWidth c) (nb+lineOffset) ) nl,Just style')
            newLines = horizontalPostProcess fl
            r = addParaLine verstate newStyle paraSettings c (zip newLines [1..])
        in
          case r of
              Left (d,c',remPara) -> (d,c',remPara ++ l')
              Right c' -> fillContainer verstate c' l'
    
fillContainer verstate c oldl@(a:l) = 
    let c' = addTo verstate a c
    in
    if isOverfull c' 
      then
          (drawContainer c,c,oldl)
      else
          fillContainer verstate c' l
          
-- | Convert pure lines to VBoxes
toVBoxes :: (ParagraphStyle ps s) => Maybe ps
       -> PDFFloat -- ^ Max width
       -> HBox s -- ^ List of lines
       -> Int -- ^ Line number
       -> VBox ps s -- ^ List of VBoxes
toVBoxes Nothing _ a _ = SomeVBox 0.0 (boxWidth a,boxHeight a,boxDescent a) (AnyBox a) Nothing
toVBoxes s@(Just style) w a nb = 
        let delta = (linePosition style) w nb in
        SomeVBox delta (boxWidth a,boxHeight a,boxDescent a) (AnyBox a) s

