---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Shapes
---------------------------------------------------------

module Graphics.PDF.Shapes(
   -- * Shapes
   -- ** Paths
     moveto
   , lineto
   , arcto
   , curveto
   , beginPath
   , closePath
   , addBezierCubic
   , addPolygonToPath
   , addLineToPath
   , strokePath
   , fillPath
   , fillAndStrokePath
   , fillPathEO
   , fillAndStrokePathEO
   , setAsClipPath
   , setAsClipPathEO
   -- ** Usual shapes
   , Shape(..)
   , Line(..)
   , Rectangle(..)
   , Polygon(..)
   , Arc(..)
   , Ellipse(..)
   , Circle(..)
   , RoundRectangle(..)
   -- ** Style
   , CapStyle(..)
   , JoinStyle(..)
   , DashPattern(..)
   , setWidth
   , setLineCap
   , setLineJoin
   , setDash
   , setNoDash
   , setMiterLimit
 ) where

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Coordinates
import Graphics.PDF.Draw
import Control.Monad.Writer
import Graphics.PDF.LowLevel.Serializer

class Shape a where
    addShape :: a -> Draw ()
    stroke :: a -> Draw ()
    fill :: a -> Draw ()
    fillAndStroke :: a -> Draw ()
    fillEO :: a -> Draw ()
    fillAndStrokeEO :: a -> Draw ()
    stroke r = do
        addShape r
        strokePath
    fill r = do
        addShape r
        fillPath
    fillAndStroke r = do
        addShape r
        fillAndStrokePath
    fillEO r = do
        addShape r
        fillPathEO
    fillAndStrokeEO r = do
        addShape r
        fillAndStrokePathEO
    
data Line = Line PDFFloat PDFFloat PDFFloat PDFFloat deriving(Eq)
instance Shape Line where
    addShape (Line x0 y0 x1 y1)= do
        moveto (x0 :+ y0)
        lineto (x1 :+ y1)
    fill _ = error "Can't fill a line !"
    fillAndStroke _ = error "Can't fill a line !"
    fillEO _ = error "Can't fill a line !"
    fillAndStrokeEO _ = error "Can't fill a line !"
    
data Rectangle = Rectangle !Point !Point deriving (Eq) 
instance Shape Rectangle where
 addShape (Rectangle a b) 
     = tell . mconcat $ [ serialize '\n'
                        , toPDF a
                        , serialize ' '
                        , toPDF (b - a)
                        , serialize " re" ]
 
data Arc = Arc PDFFloat PDFFloat PDFFloat PDFFloat deriving(Eq)
instance Shape Arc where
    addShape (Arc x0 y0 x1 y1) = do
        let height = y1 - y0
            width = x1 - x0
            kappa = 0.5522847498
        beginPath (x0 :+ y0)
        addBezierCubic ((x0+width*kappa) :+ y0) (x1 :+ (y1-height*kappa)) (x1 :+ y1)
               
data Ellipse = Ellipse PDFFloat PDFFloat PDFFloat PDFFloat deriving(Eq)
instance Shape Ellipse where
    addShape (Ellipse x0 y0 x1 y1) = do
        let xm = (x0+x1)/2.0
            ym = (y0+y1)/2.0
            k = 0.5522847498
            h = k*(abs (y1 - y0)/2.0)
            w = k*(abs (x1 - x0)/2.0)

        beginPath (xm :+ y0)
        addBezierCubic ((xm + w) :+ y0) (x1 :+ (ym - h)) (x1 :+ ym)
        addBezierCubic (x1 :+ (ym + h)) ((xm + w) :+ y1) (xm :+ y1)
        addBezierCubic ((xm - w) :+ y1) (x0 :+ (ym + h)) (x0 :+ ym)
        addBezierCubic (x0 :+ (ym - h)) ((xm - w) :+ y0) (xm :+ y0)

data RoundRectangle = RoundRectangle PDFFloat PDFFloat PDFFloat PDFFloat PDFFloat PDFFloat deriving(Eq)
instance Shape RoundRectangle where
    addShape (RoundRectangle rw rh x0 y0 x1 y1) = do
        let k = 0.5522847498
            h = k*rw
            w = k*rh

        beginPath ((x0+rw) :+ y0)
        addLineToPath ((x1-rw) :+ y0)
        addBezierCubic ((x1-rw + w) :+ y0) (x1 :+ (y0+rh - h)) (x1 :+ (y0+rh))
        addLineToPath (x1 :+ (y1-rh))
        addBezierCubic (x1 :+ (y1-rh + h)) ((x1-rw + w) :+ y1) ((x1-rw) :+ y1)
        addLineToPath ((x0+rw) :+ y1)
        addBezierCubic ((x0+rw - w) :+ y1) (x0 :+ (y1-rh + h)) (x0 :+ (y1-rh))
        addLineToPath (x0 :+ (y0+rh))
        addBezierCubic (x0 :+ (y0+rh - h)) ((x0+rw - w) :+ y0) ((x0+rw) :+ y0)
        addLineToPath ((x1-rw) :+ y0)
        
data Circle = Circle PDFFloat PDFFloat PDFFloat deriving(Eq)
instance Shape Circle where
    addShape (Circle x0 y0 r) = addShape (Ellipse (x0-r) (y0-r) (x0+r) (y0+r) )
                
newtype Polygon = Polygon [Point]
instance Shape Polygon where
    addShape (Polygon l) = addPolygonToPath l


-- | Set pen width
setWidth :: MonadPath m => PDFFloat -> m ()
setWidth w = tell . mconcat $[ serialize "\n" 
                             , toPDF w
                             , serialize " w"
                             ]

-- | Set pen width
setMiterLimit :: MonadPath m => PDFFloat -> m ()
setMiterLimit w = tell . mconcat $[ serialize "\n" 
                                  , toPDF w
                                  , serialize " M"
                                  ]

-- | Line cap styles
data CapStyle = ButtCap
              | RoundCap
              | SquareCap
              deriving(Eq,Enum)
              
-- | Line join styles
data JoinStyle = MiterJoin
               | RoundJoin
               | BevelJoin
               deriving(Eq,Enum)
                            
-- | Set line cap
setLineCap :: MonadPath m => CapStyle -> m ()
setLineCap w = tell . mconcat $[ serialize "\n " 
                               , toPDF (fromEnum  w)
                               , serialize " J"
                               ]

-- | Set line join
setLineJoin :: MonadPath m => JoinStyle -> m ()
setLineJoin w = tell . mconcat $[ serialize "\n " 
                                , toPDF (fromEnum  w)
                                , serialize " j"
                                ]

data DashPattern = DashPattern ![PDFFloat] PDFFloat deriving(Eq)

-- | Set the dash pattern
setDash :: MonadPath m => DashPattern -> m()
setDash (DashPattern a p) = 
    tell . mconcat$ [ serialize "\n " 
                    , toPDF a
                    , serialize ' '
                    , toPDF p
                    , serialize " d"
                    ]

-- | No dash pattern
setNoDash :: MonadPath m => m ()
setNoDash = setDash (DashPattern [] 0)
    
-- | Begin a new path at a position
beginPath :: Point 
          -> Draw ()
beginPath = moveto

-- | Close current path 
closePath :: Draw ()
closePath = tell . serialize $ "\nh"


-- | Append a cubic Bezier curve to the current path. The curve extends 
-- from the current point to the point (x3 , y3), using (x1 , y1 ) and 
-- (x2, y2) as the Bezier control points
addBezierCubic :: Point
               -> Point
               -> Point
               -> Draw ()
addBezierCubic b c d = do
    tell . mconcat $ [ serialize "\n" 
                     , toPDF b
                     , serialize ' '
                     , toPDF c
                     , serialize ' '
                     , toPDF d
                     , serialize " c"
                     ]
    writeDrawST penPosition d
                    
-- | Move pen to a given point without drawing anything
moveto :: Point 
       -> Draw ()
moveto a = do 
    tell . mconcat $ [ serialize "\n" 
                     , toPDF a
                     , serialize " m"
                     ]
    writeDrawST penPosition a

-- | Draw a line from current point to the one specified by lineto
lineto :: Point 
       -> Draw () 
lineto a = do
    tell . mconcat $[ serialize "\n" 
                    , toPDF a
                    , serialize " l"
                    ]
    writeDrawST penPosition a

curveto :: Point -> Point -> Point -> Draw ()
curveto = addBezierCubic

-- | Approximate a circular arc by one cubic bezier curve.
-- larger arc angles mean larger distortions
arcto :: Angle   -- ^ Extent of arc
      -> Point   -- ^ Center of arc
      -> Draw ()
arcto extent 
    = let theta = toRadian extent
          kappa = 4 / 3 * tan (theta / 4)
          cis_theta = cis theta
          rot90 (x :+ y) = ((-y) :+ x)
       in if theta == 0
          then \_center -> return ()
          else \center -> do
            a <- readDrawST penPosition
            let delta  = a - center
                delta' = scalePt kappa (rot90 delta)
                d = center + delta * cis_theta
                c = d - delta' * cis_theta
                b = a + delta'
            curveto b c d

addLineToPath :: Point 
              -> Draw ()
addLineToPath = lineto

-- | Add a polygon to current path
addPolygonToPath :: [Point]
                 -> Draw ()
addPolygonToPath []  = return ()
addPolygonToPath (l : ls) =  do
    moveto l
    mapM_ addLineToPath ls  
    
-- | Draw current path
strokePath :: Draw ()             
strokePath = tell . serialize $ "\nS"

-- | Fill current path
fillPath :: Draw ()             
fillPath = tell . serialize $ "\nf"

-- | Fill current path
fillAndStrokePath :: Draw ()             
fillAndStrokePath = tell . serialize $ "\nB"

-- | Set clipping path
setAsClipPathEO :: Draw ()             
setAsClipPathEO = tell . serialize $ "\nW* n"

-- | Set clipping path
setAsClipPath :: Draw ()             
setAsClipPath = tell . serialize $ "\nW n"

-- | Fill current path using even odd rule
fillPathEO :: Draw ()             
fillPathEO = tell . serialize $ "\nf*"

-- | Fill current path using even odd rule
fillAndStrokePathEO :: Draw ()             
fillAndStrokePathEO = tell . serialize $ "\nB*"