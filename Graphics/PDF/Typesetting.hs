{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Experimental typesetting. It is a work in progress
---------------------------------------------------------

module Graphics.PDF.Typesetting(
  -- * Types
  -- ** Boxes
    Box(..)
  , DisplayableBox(..)
  , Letter(..)
  , BoxDimension
  -- ** Styles
  , Style(..)
  , TextStyle(..)
  , StyleFunction(..)
  , ParagraphStyle(..)
  , MonadStyle(..)
  , ComparableStyle(..)
  -- ** Typesetting monads
  , Para
  , TM
  -- ** Containers
  , VBox
  , VerState(..)
  , Container
  , Justification(..)
  , Orientation(..)
  -- * Functions
  -- ** Text display
  , displayFormattedText
  -- ** Text construction operators
  , txt
  , kern
  , addPenalty
  , mkLetter
  , mkDrawBox
  -- ** Paragraph construction operators
  , forceNewLine
  , paragraph
  , endPara
  , startPara
  -- ** Functions useful to change the paragraph style
  , getParaStyle
  , setParaStyle
  -- ** Container
  , mkContainer
  , fillContainer
  , defaultVerState
  , getBoxes
  , containerX
  , containerY
  , containerWidth
  , containerHeight
  , containerContentHeight
  , containerContentRightBorder
  , containerContentLeftBorder
  , containerCurrentHeight
  , containerContentRectangle
  , drawTextBox
  -- * Settings (similar to TeX ones)
  -- ** Line breaking settings
  , setFirstPassTolerance 
  , setSecondPassTolerance
  , setHyphenPenaltyValue 
  , setFitnessDemerit
  , setHyphenDemerit
  , setLinePenalty
  , getFirstPassTolerance 
  , getSecondPassTolerance
  , getHyphenPenaltyValue 
  , getFitnessDemerit
  , getHyphenDemerit
  , getLinePenalty
  , setJustification
  -- ** Vertical mode settings
  , setBaseLineSkip
  , setLineSkipLimit
  , setLineSkip
  , getBaseLineSkip
  , getLineSkipLimit
  , getLineSkip
  , module Graphics.PDF.Typesetting.StandardStyle
  ) where
  
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Shapes
import Graphics.PDF.Coordinates
import Control.Monad.RWS
import Graphics.PDF.Typesetting.Breaking
import Graphics.PDF.Typesetting.Vertical
import Graphics.PDF.Typesetting.Layout
import Graphics.PDF.Typesetting.Box
import Graphics.PDF.Typesetting.StandardStyle
import Graphics.PDF.Hyphenate
import Data.List(unfoldr,intersperse)
import Data.Char(isSpace,isAlpha)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

-- | Display a formatted text in a given bounding rectangle with a given default paragraph style, a given default text style. No clipping
-- is taking place. Drawing stop when the last line is crossing the bounding rectangle in vertical direction
displayFormattedText :: (ParagraphStyle ps s) => Rectangle -- ^ Text area
                     -> ps -- ^ default vertical style
                     -> s -- ^ Default horizontal style
                     -> TM ps s a -- ^ Typesetting monad
                     -> Draw a -- ^ Draw monad
displayFormattedText (Rectangle (xa :+ ya) (xb :+ yb)) defaultVStyle defaultHStyle t  = 
    do
    --withNewContext $ do
    --    addShape $ Rectangle (xa-1) y' (xb+1) y''
    --    closePath
    --    setAsClipPath
        let (a, s', boxes) = (runRWS . unTM $ t >>= \x' -> do {return x'} ) () (defaultTmState defaultVStyle defaultHStyle)
            c = mkContainer xa yb (xb-xa) (yb-ya) 0
            (d,_,_) = fillContainer (pageSettings s') c boxes
        d
        return a
 
-- | Return the list of Vboxes for a text
getBoxes :: (ParagraphStyle ps s) => ps -- ^ default vertical style
         -> s -- ^ Default horizontal style
         -> TM ps s a -- ^ Typesetting monad
         -> [VBox ps s] -- ^ List of boxes
getBoxes defaultVStyle defaultHStyle t  =
    let (_, _ , boxes) = (runRWS . unTM $ t >>= \x' -> do {return x'} ) () (defaultTmState defaultVStyle defaultHStyle)
    in boxes

-- | Add a penalty
addPenalty :: Int -> Para s ()
addPenalty f = tell $ [penalty f]
    
defaultTmState :: (ParagraphStyle ps s) => ps -> s -> TMState ps s
defaultTmState s' s = TMState { tmStyle = s
                              , paraSettings = defaultBreakingSettings
                              , pageSettings = defaultVerState s'
                              }
    
data TMState ps s = TMState { tmStyle :: !s
                            , paraSettings :: !BRState
                            , pageSettings :: !(VerState ps)
                            }
                       
newtype TM ps s a = TM { unTM :: RWS () [VBox ps s] (TMState ps s) a} 
#ifndef __HADDOCK__
  deriving(Monad,Applicative,MonadWriter [VBox ps s], MonadState (TMState ps s), Functor)
#else
instance Monad TM
instance MonadWriter [VBox ps s] TM
instance MonadState (TMState ps s) TM
instance Functor TM
#endif

newtype Para s a = Para { unPara :: RWS BRState [Letter s] s a} 
#ifndef __HADDOCK__
  deriving(Monad,Applicative,MonadWriter [Letter s], MonadReader BRState, MonadState s, Functor)
#else
instance Monad Para
instance MonadWriter [Letter s] Para
instance MonadState s Para
instance Functor Para
instance MonadReader BRState Para
#endif

-- | A MonadStyle where some typesetting operators can be used
class (Style s, Monad m) => MonadStyle s m | m -> s where
    -- | Set the current text style
    setStyle :: s -> m ()
    
    -- | Get the current text style
    currentStyle :: m s
    
    -- | Add a box using the current mode (horizontal or vertical. The current style is always applied to the added box)
    addBox :: (Show a, DisplayableBox a, Box a) => a 
           -> PDFFloat -- ^ Width
           -> PDFFloat -- ^ Height
           -> PDFFloat -- ^ Descent
           -> m ()
    
    -- | Add a glue using the current style
    glue :: PDFFloat -- ^ Size of glue (width or height depending on the mode)
         -> PDFFloat -- ^ Dilatation factor
         -> PDFFloat -- ^ Compression factor
         -> m ()
    
    -- | Add a glue with no style (it is just a translation)
    unstyledGlue :: PDFFloat -- ^ Size of glue (width or height depending on the mode) 
                 -> PDFFloat -- ^ Dilatation factor 
                 -> PDFFloat -- ^ Compression factor 
                 -> m ()
    
    
instance Style s => MonadStyle s (TM ps s) where
    --  Set style of text
    setStyle f = modifyStrict $ \s -> s {tmStyle = f}

    --  Get current text style
    currentStyle = gets tmStyle
    
    --  Add a box to the stream in vertical mode
    addBox a w h d = do
        style <- getParaStyle
        tell $ ([SomeVBox 0 (w,h,d) (AnyBox a) (Just style)])
    
    --  Add a glue
    glue h y z = do
        style <- getParaStyle
        tell $ [vglue (Just style) h y z 0 0]
        
    --  Add a glue
    unstyledGlue h y z = do
        tell $ [vglue Nothing h y z 0 0]
    
instance Style s => MonadStyle s (Para s) where
    --  Set style of text
    setStyle f = put $! f

    --  Get current text style
    currentStyle = get
        
    --  Add a box to the stream in horizontal mode
    addBox a w h d = do
        f <- currentStyle
        addLetter . mkLetter (w,h,d) (Just f) $ a
    
    --  Add a glue
    glue w y z = do
        f <- currentStyle
        tell $ [glueBox (Just f) w y z]
        
    --  Add a glue
    unstyledGlue w y z = do
        tell $ [glueBox Nothing w y z]
        
-- | For a newline and end the current paragraph
forceNewLine :: Style s => Para s ()
forceNewLine = do
    endPara
    startPara
    
-- | End the current paragraph with or without using the same style
endFullyJustified :: Style s => Bool -- ^ True if we use the same style to end a paragraph. false for an invisible style
             -> Para s ()
endFullyJustified r = do
    if r
        then
            glue 0 10000.0 0
        else
            tell $ [glueBox Nothing 0 10000.0 0]
    addPenalty (-infinity)
     
endPara :: Style s => Para s ()
endPara = do
    style <- ask
    theStyle <- currentStyle
    let w = spaceWidth theStyle
    case centered style of
      Centered -> do
        addLetter (glueBox (Just theStyle) 0 (centeredDilatationFactor*w) 0)
        addLetter (penalty (-infinity))
      RightJustification -> addPenalty (-infinity) 
      _ -> endFullyJustified False
      
startPara :: Style s => Para s ()
startPara = do
    style <- ask
    theStyle <- currentStyle
    let w = spaceWidth theStyle
    case (centered style) of
      Centered -> do
        addLetter (kernBox (theStyle) 0)
        addLetter $ penalty infinity
        addLetter (glueBox (Just theStyle) 0 (centeredDilatationFactor*w) 0)
      RightJustification -> do
        addLetter (kernBox (theStyle) 0)
        addLetter $ penalty infinity
        addLetter (glueBox (Just theStyle) 0 (rightDilatationFactor*w) 0)
      _ -> return ()
      
-- | Run a paragraph. Style changes are local to the paragraph
runPara :: Style s => Para s a -> TM ps s a
runPara m = do
    TMState f settings pagesettings <- get
    let (a, s', boxes) = (runRWS . unPara $ closedPara) settings f
    put $! TMState s' settings pagesettings
    style <- getParaStyle
    tell $ [Paragraph 0 boxes (Just style) settings]
    return a
 where
    closedPara = do
        startPara
        x <- m
        endPara
        return x
    
-- | Get the current paragraph style
getParaStyle :: TM ps s ps
getParaStyle = gets pageSettings >>= TM . return . currentParagraphStyle

-- | Change the current paragraph style
setParaStyle :: ParagraphStyle ps s => ps -> TM ps s ()
setParaStyle style = do
    modifyStrict $ \s -> s {pageSettings = (pageSettings s){currentParagraphStyle = style}}

-- | Add a letter to the paragraph
addLetter :: Letter s -> Para s ()
addLetter l = Para . tell $ [l]

-- | Add a new paragraph to the text
paragraph :: Style s => Para s a -> TM ps s a
paragraph = runPara

-- | Add a null char
--nullChar :: Para ()
--nullChar = Para . tell $ [nullLetter]

myWords' :: String -> Maybe (String, String)
myWords' l  | null l = Nothing
            | otherwise = if null h then Just (h', t') else Just (" ", t)
    where 
        (h, t) = span isSpace l
        (h', t') = span (not . isSpace) l
   
-- | Split a sentence into words keeping the space but shortening them to 1 space
myWords :: String -> [String]     
myWords l = concatMap onlyWord . unfoldr myWords' $ l 
 where
  onlyWord s = let (w,p) = span isAlpha s in
     case (null w,null p) of
         (True,True) -> []
         (False,True) -> [w]
         (True,False) -> [p]
         (False,False) -> [w,p]
    
addHyphens :: HyphenationDatabase -> String -> PDFString
addHyphens db f = toPDFString . concat . map (concat . intersperse "/-" . hyphenate db) . myWords $ f
    
-- | Add a text line
txt :: Style s => String -> Para s ()
txt t = do
    f <- currentStyle
    settings <- ask
    tell $ splitText settings f (addHyphens (hyphenation settings) t)

-- | add a kern (space that can be dilated or compressed and on which no line breaking can occur)
kern :: Style s => PDFFloat -> Para s ()
kern w  = do
    f <- currentStyle
    tell $ [kernBox f w]

setBaseLineSkip :: PDFFloat -> PDFFloat -> PDFFloat -> TM ps s ()
setBaseLineSkip w y z = modifyStrict $ \s -> s {pageSettings = (pageSettings s){baselineskip = (w,y,z)}}
 
getBaseLineSkip :: TM ps s (PDFFloat,PDFFloat,PDFFloat)
getBaseLineSkip = do
    s <- gets pageSettings
    return (baselineskip s)
    
setLineSkipLimit :: PDFFloat  -> TM ps s ()
setLineSkipLimit l = modifyStrict $ \s -> s {pageSettings = (pageSettings s){lineskiplimit=l}}

getLineSkipLimit :: TM ps s PDFFloat
getLineSkipLimit = gets pageSettings >>= return . lineskiplimit

setLineSkip :: PDFFloat -> PDFFloat -> PDFFloat -> TM ps s ()
setLineSkip w y z = modifyStrict $ \s -> s {pageSettings = (pageSettings s){lineskip = (w,y,z)}}

getLineSkip :: TM ps s (PDFFloat,PDFFloat,PDFFloat)
getLineSkip = gets pageSettings >>= return . lineskip
    
setFirstPassTolerance :: PDFFloat -> TM ps s ()
setFirstPassTolerance x = modifyStrict $ \s -> s {paraSettings = (paraSettings s){firstPassTolerance = x}}

getFirstPassTolerance :: TM ps s PDFFloat
getFirstPassTolerance = gets paraSettings >>= return . firstPassTolerance

setSecondPassTolerance :: PDFFloat -> TM ps s ()
setSecondPassTolerance x = modifyStrict $ \s -> s {paraSettings = (paraSettings s){secondPassTolerance = x}}

getSecondPassTolerance :: TM ps s PDFFloat
getSecondPassTolerance = gets paraSettings >>= return . secondPassTolerance

setHyphenPenaltyValue :: Int -> TM ps s ()
setHyphenPenaltyValue x = modifyStrict $ \s -> s {paraSettings = (paraSettings s){hyphenPenaltyValue = x}}

getHyphenPenaltyValue :: TM ps s Int
getHyphenPenaltyValue = gets paraSettings >>= return . hyphenPenaltyValue

setFitnessDemerit :: PDFFloat -> TM ps s ()
setFitnessDemerit x = modifyStrict $ \s -> s {paraSettings = (paraSettings s){fitness_demerit = x}}

getFitnessDemerit :: TM ps s PDFFloat
getFitnessDemerit = gets paraSettings >>= return . fitness_demerit

setHyphenDemerit :: PDFFloat -> TM ps s ()
setHyphenDemerit x = modifyStrict $ \s -> s {paraSettings = (paraSettings s){flagged_demerit = x}}

getHyphenDemerit :: TM ps s PDFFloat
getHyphenDemerit = gets paraSettings >>= return . flagged_demerit
  
setLinePenalty :: PDFFloat -> TM ps s ()
setLinePenalty x = modifyStrict $ \s -> s {paraSettings = (paraSettings s){line_penalty = x}}
                   
getLinePenalty :: TM ps s PDFFloat
getLinePenalty = gets paraSettings >>= return . line_penalty

setJustification :: Justification -- ^ Centered, left or fully justified
                 -> TM ps s ()
setJustification j = modifyStrict $ \s -> s {paraSettings = (paraSettings s){centered = j}}

-------------------------------
--
-- Tools to ease tech drawings
--
-------------------------------

data Orientation = E | W | N | S | NE | NW | SE | SW deriving(Eq,Show)

-- | Draw a text box with relative position. Useful for labels
drawTextBox :: (ParagraphStyle ps s, Style s) 
            => PDFFloat -- ^ x
            -> PDFFloat -- ^ y
            -> PDFFloat -- ^ width limit
            -> PDFFloat -- ^ height limit
            -> Orientation
            -> ps -- ^ default vertical style
            -> s -- ^ Default horizontal style
            -> TM ps s a -- ^ Typesetting monad
            -> (Rectangle,Draw ())
drawTextBox x y w h ori ps p t = 
    let b = getBoxes ps p t
        sh = styleHeight p
        c = mkContainer 0 0 w h sh
        (d,c',_) = fillContainer (defaultVerState ps) c b
        Rectangle (xa :+ ya) (xb :+ yb)  = containerContentRectangle  c'
        wc = xb - xa
        hc = yb - ya
        (dx,dy) = case ori of
          NE -> (x,y)
          NW -> (x - wc,y)
          SE -> (x,y + hc)
          SW -> (x - wc,y + hc)
          E -> (x,y + hc / 2.0)
          W -> (x - wc,y + hc / 2.0)
          N -> (x - wc/2.0,y)
          S -> (x - wc/2.0,y + hc)
        box = withNewContext $ do
    	   applyMatrix $ translate (dx :+ dy)
    	   d
        r = Rectangle ((xa + dx) :+ (ya + dy)) ((xb + dx) :+ (yb + dy))
    in
    (r,box)
