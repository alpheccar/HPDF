{-# LANGUAGE CPP #-}
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
   , startNewLine
   , displayGlyphs
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
   , pdfGlyph
   , glyph
 ) where

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Control.Monad.State
import Graphics.PDF.Resources
import Control.Monad.Writer
import qualified Data.Set as Set
import Data.List(foldl')
import Data.Binary.Builder(Builder)
import Graphics.PDF.LowLevel.Serializer
import qualified Data.ByteString as S
import qualified Data.Text as T
import Data.Char
import Graphics.PDF.Font


glyphStreamWidth :: IsFont f 
                 => f 
                 -> PDFGlyph 
                 -> PDFFloat
glyphStreamWidth f (PDFGlyph t) = 
 let w = foldl' (\a b -> a + glyphWidth f (fromIntegral b)) 0 . S.unpack $ t
 in
  w + (foldl' (\a (x,y) -> a + getKern f x y) 0 $ [(GlyphCode ca,GlyphCode cb) | (ca,cb) <- S.zip t (S.tail t)])

textWidth :: IsFont f => f -> T.Text -> PDFFloat
textWidth f t = glyphStreamWidth f . pdfGlyph f $ t
      
pdfGlyph :: IsFont f 
         => f 
         -> T.Text 
         -> PDFGlyph 
pdfGlyph f t = PDFGlyph . S.pack . map (fromIntegral . charGlyph f) . T.unpack $ t 


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


glyph :: GlyphCode -> PDFGlyph 
glyph c = PDFGlyph . S.singleton $ (fromIntegral c)

-- | Display glyphs
displayGlyphs :: PDFGlyph
              -> PDFText ()
displayGlyphs t = do
    tell $ serialize ' '
    tell . toPDF $ t
    tell . serialize $ " Tj"

-- | Display text
displayText :: T.Text
            -> PDFText ()
displayText t = do
    f <- gets currentFont
    let g = pdfGlyph f t
    displayGlyphs g


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
     -> T.Text
     -> PDFText ()
text f x y t = do
    setFont f
    let g = pdfGlyph f t
    textStart x y
    displayGlyphs g
    
