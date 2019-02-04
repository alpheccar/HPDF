---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Coordinates for a PDF document
---------------------------------------------------------

module Graphics.PDF.Coordinates
    ( module Data.Complex
    -- * Geometry
    -- ** Types
    , Angle(..)
    , Point
    , Matrix(..)
    -- ** Transformations
    , toRadian
    , dot, scalePt
    , project, projectX, projectY
    , pointMatrix
    , transform
    , identity, rotate, translate, scale, spiral
    )
    where

import Data.Complex
import Graphics.PDF.LowLevel.Types(PDFFloat)

-- | Angle 
data Angle = Degree !PDFFloat -- ^ Angle in degrees
           | Radian !PDFFloat -- ^ Angle in radians

toRadian :: Angle -> PDFFloat
toRadian (Degree x) = (pi / 180) * x
toRadian (Radian x) = x

type Point = Complex PDFFloat

-- | Dot product of two points
-- 'dot (x :+ y) (a :+ b) == x * a  +  y * b'
-- 'dot z w == magnitude z * magnitude w * cos (phase z - phase w)'
dot :: (RealFloat t) => Complex t -> Complex t -> t
dot (x0 :+ y0) (x1 :+ y1) = x0 * x1 + y0 * y1

scalePt :: (RealFloat t) => t -> Complex t -> Complex t
scalePt a (x :+ y) = a*x :+ a*y 

-- | projects the first point onto the second
project :: (RealFloat t) => Complex t -> Complex t -> Complex t
project z w =  scalePt (dot z w / dot w w) w

-- | projects a point onto the x-axis
projectX :: (RealFloat t) => Complex t -> Complex t
projectX (x :+ _) = (x :+ 0)

-- | projects a point onto the y-axis
projectY :: (RealFloat t) => Complex t -> Complex t
projectY (_ :+ y) = (0 :+ y)

-- | A transformation matrix. An affine transformation a b c d e f
--
-- @
-- a b 0
-- c d 0
-- e f 1
-- @
       
data Matrix = Matrix !PDFFloat !PDFFloat !PDFFloat !PDFFloat !PDFFloat !PDFFloat deriving (Eq, Show)
		
instance Num Matrix where
    --  Matrix addition
    (+) (Matrix ma mb mc md me mf ) (Matrix na nb nc nd ne nf) = 
         Matrix (ma+na)  (mb+nb)  (mc+nc)  (md+nd)  (me+ne)  (mf+nf)
    (*) (Matrix ma mb mc md me mf) (Matrix na nb nc nd ne nf) = 
         Matrix (ma*na+mb*nc)  (ma*nb+mb*nd)  (mc*na+md*nc)  (mc*nb +md*nd)  (me*na+mf*nc+ne)  (me*nb+mf*nd+nf)
    negate (Matrix ma mb mc md me mf )  =
         Matrix (-ma)  (-mb)  (-mc)  (-md)  (-me)  (-mf)
    abs m = m
    signum _ = identity
    fromInteger i = Matrix r 0 0 r  0  0
                   where
                    r = fromInteger i

-- | Identity matrix
identity :: Matrix
identity = Matrix 1 0 0 1 0 0

-- | Specifies a matrix as three points
pointMatrix :: Point   -- ^ X component
            -> Point   -- ^ Y component
            -> Point   -- ^ translation component
            -> Matrix
pointMatrix (x0 :+ y0) (x1 :+ y1) (x2 :+ y2) = Matrix x0 y0 x1 y1 x2 y2

-- | Applies a matrix to a point
transform :: Matrix -> Point -> Point
transform (Matrix x0 y0 x1 y1 x2 y2) (x :+ y) = (x*x0 + y*x1 + x2) :+ (x*y0 + y*y1 + y2)

                    
-- | Rotation matrix
rotate :: Angle -- ^ Rotation angle
       -> Matrix
rotate r = spiral (cis (toRadian r))

-- | Translation matrix
-- 'transform (translate z) w == z + w'             
translate :: Point 
          -> Matrix
translate (tx :+ ty)  = Matrix  1  0  0  1  tx ty

--  | 'Spiral z' rotates by 'phase z' and scales by 'magnitude z'
--  'transform (spiral z) w == z * w'
spiral :: Point 
       -> Matrix
spiral (x :+ y) = Matrix x y (-y) x 0 0


-- | Scaling matrix          
scale :: PDFFloat  -- ^ Horizontal scaling
      -> PDFFloat  -- ^ Horizontal scaling
      -> Matrix
scale sx sy  = Matrix sx 0 0 sy 0 0