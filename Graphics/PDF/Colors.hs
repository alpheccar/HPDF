---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Colors for a PDF document
---------------------------------------------------------
module Graphics.PDF.Colors(
  -- * Colors
  -- ** Types
    Color(..)
  -- ** Functions
  , setRGBColorSpace
  , fillColor
  , strokeColor
  , setStrokeAlpha
  , setFillAlpha
  , hsvToRgb
  -- ** Some colors
  , black
  , white
  , red
  , blue
  , green
 ) where
     
import Graphics.PDF.Draw
import Graphics.PDF.LowLevel.Types
import Control.Monad.State(gets)
import Graphics.PDF.Resources
import Control.Monad.Writer
import Graphics.PDF.LowLevel.Serializer
            
black :: Color
black = Rgb 0 0 0  

white :: Color
white = Rgb 1 1 1

red :: Color
red = Rgb 1 0 0

green :: Color
green = Rgb 0 1 0

blue :: Color
blue = Rgb 0 0 1
            

       
-- | Set alpha value for transparency
setStrokeAlpha :: Double -> Draw ()
setStrokeAlpha alpha = do
    alphaMap <- gets strokeAlphas
    (newName,newMap) <- setResource "ExtGState" (StrokeAlpha alpha) alphaMap
    modifyStrict $ \s -> s { strokeAlphas = newMap }
    tell . mconcat $[ serialize "\n/" 
                    , serialize newName
                    , serialize " gs"
                    ]
        
-- | Set alpha value for transparency
setFillAlpha :: Double -> Draw ()
setFillAlpha alpha = do
    alphaMap <- gets fillAlphas
    (newName,newMap) <- setResource "ExtGState" (FillAlpha alpha) alphaMap
    modifyStrict $ \s -> s { fillAlphas = newMap }
    tell . mconcat $[ serialize "\n/" 
                    , serialize newName
                    , serialize " gs"
                    ]
    
-- | Init the PDF color space to RGB.
setRGBColorSpace :: Draw ()
setRGBColorSpace = tell . serialize $ "\n/DeviceRGB CS\n/DeviceRGB cs\n"



-- | Select the filling color
fillColor :: MonadPath m => Color -- ^ Filling color
          -> m ()
fillColor (Rgb r g b) = do
    tell . mconcat $[ serialize "\n"
                    , toPDF r
                    , serialize ' '
                    , toPDF g
                    , serialize ' '
                    , toPDF b
                    , serialize " rg" 
                    ]
                    
fillColor (Hsv h s v) = do
        let (r,g,b) = hsvToRgb (h,s,v)
        tell . mconcat $[ serialize "\n"
                        , toPDF r
                        , serialize ' '
                        , toPDF g
                        , serialize ' '
                        , toPDF b
                        , serialize " rg" 
                        ]

-- | Select the drawing color
strokeColor :: MonadPath m => Color -- ^ Drawing color
            -> m ()
strokeColor (Rgb r g b) = do
    tell . mconcat $[ serialize "\n"
                    , toPDF r
                    , serialize ' '
                    , toPDF g
                    , serialize ' '
                    , toPDF b
                    , serialize " RG" 
                    ]
strokeColor (Hsv h s v) = do
    let (r,g,b) = hsvToRgb (h,s,v)
    tell . mconcat $[ serialize "\n"
                    , toPDF r
                    , serialize ' '
                    , toPDF g
                    , serialize ' '
                    , toPDF b
                    , serialize " RG" 
                    ]

