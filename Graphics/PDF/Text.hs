{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
-- PDF Text
---------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Graphics.PDF.Text(
   -- * Text
   -- ** Types
     PDFFont(..)
   , FontName(..)
   , TextMode(..)
   , PDFText
   , UnscaledUnit
   -- ** Functions
   , drawText
   , text
   , toPDFString
   , startNewLine
   , displayText
   , textStart
   , setFont
   , leading
   , charSpace
   , wordSpace
   , textScale
   , renderMode
   , rise
   , setTextMatrix
   , textWidth
   , getDescent
   , getHeight
   , ripText
   , charWidth
 ) where

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Control.Monad.State
import Graphics.PDF.Resources
import Control.Monad.Writer
import qualified Data.Set as Set
import Data.Word
import Graphics.PDF.LowLevel.Kern(kerns)
import qualified Data.Map as M(findWithDefault)
import Data.List(foldl')
import Data.Binary.Builder(Builder)
import Graphics.PDF.LowLevel.Serializer
import qualified Data.ByteString as S
#if __GLASGOW_HASKELL__ >= 608
import Data.ByteString.Internal(w2c,c2w)
#else
import Data.ByteString.Base(w2c,c2w)
#endif
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

foreign import ccall "ctext.h c_getLeading" cgetLeading :: Int -> Int
foreign import ccall "ctext.h c_getAdvance" cgetAdvance :: Int -> Int -> Int
foreign import ccall "ctext.h c_getDescent" cgetDescent :: Int -> Int
foreign import ccall "ctext.h c_hasKern" hasKern :: Int -> Bool

-- pixel size / 2048 gives factor 

-- | Convert a dimension in font unit to device unit
trueSize :: Int -> Int -> PDFFloat
trueSize fontSize textSize = (fromIntegral (textSize*fontSize)) / 1000.0

getDescent :: PDFFont -> PDFFloat
getDescent (PDFFont n s) = trueSize s (cgetDescent (fromEnum n))

getHeight :: PDFFont -> PDFFloat
getHeight (PDFFont n s) = trueSize s (cgetLeading (fromEnum n))

-- | Get the kern value for a given font and pair of charcode
getKern :: (Int,Word8,Word8) -> Int
getKern k = M.findWithDefault 0 k kerns

textWidth :: PDFFont -> PDFString -> PDFFloat
textWidth (PDFFont n s) (PDFString t) = 
 let w = foldl' (\a b -> a + cgetAdvance (fromEnum n) (fromIntegral b)) 0 . S.unpack $ t
 in
 if hasKern (fromEnum n)
   then
      trueSize s (w + (foldl' (\a b -> a + getKern b) 0 $ [(fromEnum n,ca,cb) | (ca,cb) <- S.zip t (S.tail t)]))
   else
      trueSize s w
      
charWidth :: PDFFont -> Char -> PDFFloat
charWidth (PDFFont n s) c = let w = cgetAdvance (fromEnum n) (fromEnum c) in
    trueSize s w
    
c2i :: Char -> Int
c2i = fromEnum
      
ripText :: PDFFont -- ^ Font
        -> PDFString -- ^ String
        -> [(PDFFloat,Char)] -- ^ List of chars and char width taking into account kerning
ripText (PDFFont n s) (PDFString t) = getLetters (hasKern (fromEnum n)) . S.unpack  $ t
  where
      getLetters _ [] = []
      getLetters _ [a] = [(trueSize s $ cgetAdvance (fromEnum n) (fromEnum a),w2c a)]
      getLetters False (a:l) = (trueSize s $ cgetAdvance (fromEnum n) (fromEnum a),w2c a) : getLetters False l
      getLetters True (a:b:c:d:l)  | b == (c2w '/') && c == (c2w '-') = 
                                       let k = getKern (fromEnum n,a,d)
                                           kh = getKern (fromEnum n,a,c2w '-')
                                           hw = cgetAdvance (fromEnum n) (c2i '-') 
                                       in
                                       -- We record the hyphen size + an adaptation due to the different kerning with an hyphen
                                       (trueSize s $ cgetAdvance (fromEnum n) (fromEnum a) + k,w2c a):(0,'/'):(trueSize s $ hw-k+kh,'-'):getLetters True (d:l)
                                   | otherwise = (trueSize s $ cgetAdvance (fromEnum n) (fromEnum a) + getKern (fromEnum n,a,b),w2c a) : getLetters True (b:c:d:l)
      getLetters True (a:b:l) = (trueSize s $ cgetAdvance (fromEnum n) (fromEnum a) + getKern (fromEnum n,a,b),w2c a) : getLetters True (b:l)
          
type FontState = (Set.Set FontName)

data TextParameter = TextParameter { tc :: !PDFFloat
                                   , tw :: !PDFFloat
                                   , tz :: !PDFFloat
                                   , tl :: !PDFFloat
                                   , ts :: !PDFFloat
                                   , fontState :: FontState
                                   , currentFont :: PDFFont
                                   }
defaultParameters :: TextParameter
defaultParameters = TextParameter 0 0 100 0 0 (Set.empty) (PDFFont Times_Roman 12)

     
-- | The text monad 
newtype PDFText a = PDFText {unText :: WriterT Builder (State TextParameter) a}          
#ifndef __HADDOCK__                    
  deriving(Monad,Applicative,Functor,MonadWriter Builder,MonadState TextParameter)
#else
instance Monad PDFText
instance Functor PDFText
instance MonadWriter Builder PDFText
instance MonadState TextParameter PDFText
#endif
    
instance MonadPath PDFText

-- | Unscaled unit (not scaled by the font size)
type UnscaledUnit = PDFFloat  

-- | Rendering mode for text display
data TextMode = FillText
              | StrokeText
              | FillAndStrokeText
              | InvisibleText
              | FillTextAndAddToClip
              | StrokeTextAndAddToClip
              | FillAndStrokeTextAndAddToClip
              | AddToClip
              deriving(Eq,Ord,Enum)

-- | Select a font to use
setFont :: PDFFont -> PDFText ()
setFont f@(PDFFont n size) = PDFText $ do
    lift (modifyStrict $ \s -> s {fontState = Set.insert n (fontState s), currentFont = f})
    tell . mconcat$ [ serialize "\n/" 
                    , serialize (show n)
                    , serialize ' '
                    , toPDF size
                    , serialize " Tf"
                    ]
                    
  
-- | Draw a text in the draw monad
drawText :: PDFText a
         -> Draw a
drawText t = do
    let ((a,w),s) = (runState . runWriterT . unText $ t) defaultParameters
    mapM_ addFontRsrc (Set.elems (fontState s))
    tell . serialize $ "\nBT"
    tell w
    tell . serialize $ "\nET"
    return a
 where
   addFontRsrc f = modifyStrict $ \s ->
       s { rsrc = addResource (PDFName "Font") (PDFName (show f)) (toRsrc (PDFFont f 0)) (rsrc s)}
   
-- | Set position for the text beginning
textStart :: PDFFloat
          -> PDFFloat
          -> PDFText ()
textStart x y = tell . mconcat  $ [ serialize '\n'
                                  , toPDF x
                                  , serialize ' '
                                  , toPDF y
                                  , serialize " Td"
                                  ]
 --writeCmd $ "\n" ++ (show x) ++ " " ++ (show y) ++ " Td"         

-- | Display some text
displayText :: PDFString
            -> PDFText ()
displayText t = do
    tell . toPDF $ t
    tell . serialize $ " Tj"
--    f <- gets currentFont
--    let rt = ripText f t
--    tell . serialize $ '\n'
--    tell lbracket
--    mapM_ displayGlyphs rt
--    tell rbracket
--    tell $ serialize " TJ"
-- where
-- 	displayGlyphs (w,c) = do
-- 		tell $ toPDF (toPDFString $ c:[])
-- 		tell bspace
-- 		tell . toPDF $ w
-- 		tell bspace

 
-- | Start a new line (leading value must have been set)
startNewLine :: PDFText ()
startNewLine = tell . serialize $ "\nT*"    

-- | Set leading value
leading :: UnscaledUnit -> PDFText ()
leading v = PDFText $ do
    lift (modifyStrict $ \s -> s {tl = v})
    tell . mconcat $  [ serialize '\n'
                      , toPDF v
                      , serialize " TL"
                      ]

-- | Set the additional char space
charSpace :: UnscaledUnit -> PDFText ()
charSpace v = PDFText $ do
    lift (modifyStrict $ \s -> s {tc = v})
    tell . mconcat  $ [ serialize '\n'
                      , toPDF v
                      , serialize " Tc"
                      ]

-- | Set the additional word space
wordSpace :: UnscaledUnit -> PDFText ()
wordSpace v = PDFText $ do
    lift (modifyStrict $ \s -> s {tw = v})
    tell . mconcat $ [ serialize '\n'
                      , toPDF v
                      , serialize " Tw"
                      ]

-- | Set scaling factor for text
textScale :: PDFFloat -> PDFText ()
textScale v = PDFText $ do
    lift (modifyStrict $ \s -> s {tz = v})
    tell . mconcat  $ [ serialize '\n'
                      , toPDF v
                      , serialize " Tz"
                      ]

-- | Choose the text rendering mode
renderMode :: TextMode -> PDFText ()
renderMode v = 
    tell . mconcat $  [ serialize '\n'
                      , toPDF (fromEnum v)
                      , serialize " Tr"
                      ]

-- | Set the rise value
rise :: UnscaledUnit -> PDFText ()
rise v = PDFText $ do
    lift (modifyStrict $ \s -> s {ts = v})
    tell . mconcat $  [ serialize '\n'
                      , toPDF v
                      , serialize " Ts"
                      ]

-- | Set the text transformation matrix
setTextMatrix :: Matrix -> PDFText()
setTextMatrix (Matrix a b c d e f) = 
    tell . mconcat $[ serialize '\n'
                    , toPDF a
                    , serialize ' '
                    , toPDF b
                    , serialize ' '
                    , toPDF c
                    , serialize ' '
                    , toPDF d
                    , serialize ' '
                    , toPDF e
                    , serialize ' '
                    , toPDF f
                    , serialize " Tm"
                    ]
    
-- | Utility function to quickly display one line of text
text :: PDFFont
     -> PDFFloat
     -> PDFFloat
     -> PDFString
     -> PDFText ()
text f x y t = do
    setFont f
    textStart x y
    displayText t
    
