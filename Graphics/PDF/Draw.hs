{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF API for Haskell
---------------------------------------------------------
-- #hide
module Graphics.PDF.Draw(
 -- * Draw monad
   Draw
 , PDFStream(..)
 , withNewContext
 , DrawState(..)
 , DrawEnvironment(..)
 , readDrawST 
 , writeDrawST 
 , modifyDrawST 
 , DrawTuple()
 , penPosition
 , supplyName
 , emptyDrawing
-- , writeCmd
 , runDrawing
 , setResource
 , emptyEnvironment
 , PDFXForm
 , PDFXObject(..)
 , AnyPdfXForm
 , pdfDictMember
 -- PDF types
 , PDF(..)
 , PDFPage(..)
 , PDFPages(..)
 , PdfState(..)
 , PDFCatalog(..)
 , Pages(..)
 , PDFDocumentPageMode(..)
 , PDFDocumentPageLayout(..)
 , PDFViewerPreferences(..)
 , PDFDocumentInfo(..)
 -- ** Page transitions
 , PDFTransition(..)
 , PDFTransStyle(..)
 , PDFTransDirection(..)
 , PDFTransDimension(..)
 , PDFTransDirection2(..)
 -- ** Outlines
 , PDFOutline(..)
 , OutlineStyle(..)
 , PDFOutlineEntry(..)
 , Destination(..)
 , Outline
 , OutlineLoc(..)
 , Tree(..)
 , OutlineCtx(..)
 , AnnotationObject(..)
 , Color(..)
 , hsvToRgb
 , OutlineData
 , AnyAnnotation(..)
 , AnnotationStyle(..)
 , PDFShading(..)
 , getRgbColor
 , emptyDrawState
 , Matrix(..)
 , identity
 , applyMatrix
 , currentMatrix
 , multiplyCurrentMatrixWith
 , PDFGlobals(..)
 ) where
 
import Data.Maybe
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Binary.Builder as BU
import qualified Data.ByteString.Lazy as B

import Control.Monad.ST
import Data.STRef

import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.State

import Graphics.PDF.Coordinates
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.LowLevel.Serializer
import Graphics.PDF.Resources
import Graphics.PDF.Data.PDFTree(PDFTree)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

data AnnotationStyle = AnnotationStyle !(Maybe Color)

class AnnotationObject a where
    addAnnotation :: a -> PDF (PDFReference a)
    annotationType :: a -> PDFName
    annotationContent :: a -> PDFString
    annotationRect :: a -> [PDFFloat]
    annotationToGlobalCoordinates :: a -> Draw a
    annotationToGlobalCoordinates = return
    
data AnyAnnotation = forall a.(PdfObject a,AnnotationObject a) => AnyAnnotation a

instance PdfObject AnyAnnotation where
    toPDF (AnyAnnotation a) = toPDF a
instance PdfLengthInfo AnyAnnotation where

instance AnnotationObject AnyAnnotation where
    addAnnotation (AnyAnnotation a) = do
        PDFReference r <- addAnnotation a
        return (PDFReference r)
    annotationType (AnyAnnotation a) = annotationType a
    annotationContent (AnyAnnotation a) = annotationContent a
    annotationRect (AnyAnnotation a) = annotationRect a
    

-- | A PDF color
data Color = Rgb !Double !Double !Double
           | Hsv !Double !Double !Double
           deriving(Eq,Ord)

data DrawState = DrawState {
                   supplyNames :: [String]
                ,  rsrc :: PDFResource
                ,  strokeAlphas :: M.Map StrokeAlpha String
                ,  fillAlphas :: M.Map FillAlpha String
                ,  theFonts :: M.Map PDFFont String
                ,  xobjects :: M.Map (PDFReference AnyPdfXForm) String
                ,  otherRsrcs :: PDFDictionary
                ,  annots :: [AnyAnnotation]
                ,  patterns :: M.Map (PDFReference AnyPdfPattern) String
                ,  colorSpaces :: M.Map PDFColorSpace String
                ,  shadings :: M.Map PDFShading String
                ,  matrix :: [Matrix]
                }
data DrawEnvironment = DrawEnvironment {
                        streamId :: Int
                     ,  xobjectBoundD :: IM.IntMap (PDFFloat,PDFFloat)
                     }   

data DrawTuple s
   = DrawTuple {  drawEnvironment    :: DrawEnvironment
               ,  drawStateRef  :: STRef s DrawState
               ,  builderRef :: STRef s BU.Builder
               ,  penPosition :: STRef s Point
               }
    
emptyEnvironment :: DrawEnvironment
emptyEnvironment = DrawEnvironment 0 IM.empty

class PDFGlobals m where
    bounds :: PDFXObject a => PDFReference a -> m (PDFFloat,PDFFloat)
    
-- | The drawing monad
newtype Draw a = Draw {unDraw :: forall s. DrawTuple s -> ST s a }

instance Applicative Draw where
    pure x = Draw $ \_env -> return x
    df <*> af = Draw $ \env -> do
       f <- unDraw df env
       a <- unDraw af env
       return $ f a


instance Monad Draw where
    m >>= f  = Draw $ \env -> do
                          a <- unDraw m env
                          unDraw (f a) env
    return x = Draw $ \_env -> return x

instance MonadReader DrawEnvironment Draw where
   ask       = Draw $ \env -> return (drawEnvironment env)
   local f m = Draw $ \env -> let drawenv' = f (drawEnvironment env)
                                  env' = env { drawEnvironment = drawenv' }
                               in unDraw m env' 

instance MonadState DrawState Draw where
    get    = Draw $ \env -> readSTRef  (drawStateRef env)
    put st = Draw $ \env -> writeSTRef (drawStateRef env) st

instance MonadWriter BU.Builder Draw where
    tell bu  = Draw $ \env -> modifySTRef (builderRef env) (`mappend` bu)
    listen m = Draw $ \env -> do
                 a <- unDraw m env
                 w <- readSTRef (builderRef env)
                 return (a,w)
    pass   m = Draw $ \env -> do
                 (a, f) <- unDraw m env
                 modifySTRef (builderRef env) f
                 return a

instance Functor Draw where
     fmap f = \m -> do { a <- m; return (f a) }

instance MonadPath Draw

readDrawST :: (forall s. DrawTuple s -> STRef s a) -> Draw a
readDrawST   f   = Draw $ \env -> readSTRef   (f env) 

writeDrawST :: (forall s. DrawTuple s -> STRef s a) -> a -> Draw ()
writeDrawST  f x = Draw $ \env -> writeSTRef  (f env) x 

modifyDrawST :: (forall s. DrawTuple s -> STRef s a) -> (a -> a) -> Draw ()
modifyDrawST f g = Draw $ \env -> modifySTRef (f env) g

-- | A PDF stream object
data PDFStream = PDFStream !BU.Builder !Bool !(PDFReference MaybeLength) !PDFDictionary
                                   
instance PdfObject PDFStream where
  toPDF (PDFStream s c l d) = 
      mconcat   $ [ toPDF dict
                  , serialize "\nstream"
                  , newline
                  , s
                  , newline
                  , serialize "endstream"]
   where
      compressedStream False = []
      compressedStream True = if not (pdfDictMember (PDFName "Filter") d) then [(PDFName "Filter",AnyPdfObject $ [AnyPdfObject . PDFName $ "FlateDecode"])] else []
      lenDict = PDFDictionary. M.fromList $ [ (PDFName "Length",AnyPdfObject l)] ++ compressedStream c
      dict = pdfDictUnion lenDict d

instance PdfLengthInfo PDFStream where 
  pdfLengthInfo (PDFStream s _ l _) = Just (B.length . BU.toLazyByteString $ s,l)
    
-- | An empty drawing
emptyDrawing :: Draw ()
emptyDrawing = return ()
  
-- | is member of the dictionary
pdfDictMember :: PDFName -> PDFDictionary -> Bool
pdfDictMember k (PDFDictionary d)  = M.member k d

-- | Get a new resource name
supplyName :: Draw String
supplyName = do
    (x:xs) <- gets supplyNames
    modifyStrict $ \s -> s {supplyNames = xs}
    return x
    
emptyDrawState :: Int -> DrawState
emptyDrawState ref = 
    let names = (map (("O" ++ (show ref)) ++ ) $ [replicate k ['a'..'z'] | k <- [1..]] >>= sequence) in
    DrawState names emptyRsrc M.empty M.empty M.empty M.empty emptyDictionary []  M.empty M.empty M.empty [identity]
  
-- | Execute the drawing commands to get a new state and an uncompressed PDF stream
runDrawing :: Draw a -> DrawEnvironment -> DrawState -> (a,DrawState,BU.Builder)
runDrawing drawing environment drawState 
    = runST $ do
        dRef <- newSTRef drawState
        bRef <- newSTRef mempty
        posRef <- newSTRef 0
        let tuple = DrawTuple { drawEnvironment = environment
                              , drawStateRef    = dRef
                              , builderRef      = bRef
                              , penPosition     = posRef
                              } 
        a <- unDraw drawing tuple
        drawSt <- readSTRef (drawStateRef tuple)
        builder <- readSTRef (builderRef tuple)
        return (a, drawSt, builder)
     
pushMatrixStack :: Matrix -> Draw ()
pushMatrixStack m = do
    modifyStrict $ \s -> s {matrix = m : matrix s}
    
popMatrixStack :: Draw ()
popMatrixStack = do
    modifyStrict $ \s -> s {matrix = tail (matrix s)}
    

multiplyCurrentMatrixWith :: Matrix -> Draw ()
multiplyCurrentMatrixWith m' = modifyStrict $ \s -> s {matrix = let (m:l) = matrix s in (m' * m ):l}

    
currentMatrix :: Draw Matrix
currentMatrix = gets matrix >>= return . head
      
-- | Draw in a new drawing context without perturbing the previous context
-- that is restored after the draw       
withNewContext :: Draw a -> Draw a
withNewContext m = do
    tell . serialize $ "\nq"
    pushMatrixStack identity
    a <- m
    popMatrixStack
    tell . serialize $ "\nQ"
    return a
    
-- | Set a resource in the resource dictionary
setResource :: (Ord a, PdfResourceObject a) => String -- ^ Dict name
            -> a -- ^ Resource value
            -> M.Map a String -- ^ Old cache value
            -> Draw (String,M.Map a String) -- ^ New cache value
setResource dict values oldCache = do
    case M.lookup values oldCache of
        Nothing -> do
             newName <- supplyName
             modifyStrict $ \s -> s { rsrc = addResource (PDFName dict) (PDFName newName) (toRsrc values) (rsrc s)}
             return (newName,M.insert values newName oldCache)
        Just n -> return (n,oldCache)

instance PDFGlobals Draw where
    bounds (PDFReference r) = getBoundInDraw r
    
instance PDFGlobals PDF where
    bounds (PDFReference r) = getBoundInPDF r
    
-- | A PDF Xobject which can be drawn
class PDFXObject a where
    drawXObject :: PDFReference a -> Draw ()
    
    privateDrawXObject :: PDFReference a -> Draw ()
    privateDrawXObject (PDFReference r) = do
        xobjectMap <- gets xobjects
        (newName,newMap) <- setResource "XObject" (PDFReference r) xobjectMap
        modifyStrict $ \s -> s { xobjects = newMap }
        tell . mconcat  $ [ serialize "\n/" 
                          , serialize newName
                          , serialize " Do"
                          ]
    drawXObject = privateDrawXObject
    
-- | An XObject
data AnyPdfXForm = forall a. (PDFXObject a,PdfObject a) => AnyPdfXForm a
instance PdfObject AnyPdfXForm where
    toPDF (AnyPdfXForm a) = toPDF a
instance PdfLengthInfo AnyPdfXForm where

instance PDFXObject AnyPdfXForm

data PDFXForm
instance PDFXObject PDFXForm
instance PdfObject PDFXForm where
    toPDF _ = noPdfObject
instance PdfLengthInfo PDFXForm where

instance PdfResourceObject (PDFReference PDFXForm) where
    toRsrc = AnyPdfObject
    
instance PdfResourceObject (PDFReference AnyPdfXForm) where
    toRsrc = AnyPdfObject
    

-- | Get the bounds for an xobject
getBoundInDraw :: Int -- ^ Reference
         -> Draw (PDFFloat,PDFFloat)  
getBoundInDraw ref = do
    theBounds <- asks xobjectBoundD
    return $ IM.findWithDefault (0.0,0.0) ref theBounds
 
-- | Get the bounds for an xobject
getBoundInPDF :: Int -- ^ Reference
              -> PDF (PDFFloat,PDFFloat)  
getBoundInPDF ref = do
    theBounds <- gets xobjectBound
    return $ IM.findWithDefault (0.0,0.0) ref theBounds
   
-----------
--
-- PDF types
--
------------

-- | The PDF Catalog
data PDFCatalog = PDFCatalog 
                   !(Maybe (PDFReference PDFOutline))
                   !(PDFReference PDFPages)
                   !PDFDocumentPageMode
                   !PDFDocumentPageLayout
                   !PDFViewerPreferences

-- | The PDF state
data PdfState = PdfState { supplySrc :: !Int -- ^ Supply of unique identifiers
                         , objects :: !(IM.IntMap AnyPdfObject) -- ^ Dictionary of PDF objects
                         , pages :: !Pages -- ^ Pages
                         , streams :: !(IM.IntMap ((Maybe (PDFReference PDFPage)),(DrawState,BU.Builder))) -- ^ Draw commands
                         , catalog :: !(PDFReference PDFCatalog) -- ^ Reference to the PDF catalog
                         , defaultRect :: !PDFRect -- ^ Default page size
                         , docInfo :: !PDFDocumentInfo -- ^ Document infos
                         , outline :: Maybe Outline -- ^ Root outline
                         , currentPage :: Maybe (PDFReference PDFPage) -- ^ Reference to the current page used to create outlines
                         , xobjectBound :: !(IM.IntMap (PDFFloat,PDFFloat)) -- ^ Width and height of xobjects
                         , firstOutline :: [Bool] -- ^ Used to improve the outline API
                         }
                         
-- | A PDF Page object
#ifndef __HADDOCK__
data PDFPage = PDFPage 
          !(Maybe (PDFReference PDFPages)) --  Reference to parent
          !(PDFRect) -- Media box
          !(PDFReference PDFStream) -- Reference to content
          !(Maybe (PDFReference PDFResource)) -- Reference to resources
          !(Maybe PDFFloat) -- Optional duration
          !(Maybe PDFTransition) -- Optional transition
          ![AnyPdfObject] -- Annotation array
#else
data PDFPage
#endif

instance Show PDFPage where
    show _ = "PDFPage"
    
-- | List of all pages
newtype Pages = Pages (PDFTree PDFPage)

-- | PDF Pages
#ifndef __HADDOCK__
data PDFPages = PDFPages 
              !Int
              !(Maybe (PDFReference PDFPages)) -- Reference to parent 
              [Either (PDFReference PDFPages) (PDFReference PDFPage)]
#else
data PDFPages
#endif

-- | A PDF Transition
data PDFTransition = PDFTransition !PDFFloat !PDFTransStyle  
  deriving(Eq)


-- | Dimension of a transition
data PDFTransDimension = Horizontal | Vertical 
 deriving(Eq)


instance Show PDFTransDimension where
    show Horizontal = "H"
    show Vertical = "V"

-- | Direction of a transition
data PDFTransDirection = Inward | Outward deriving(Eq)

instance Show PDFTransDirection where
    show Inward = "I"
    show Outward = "O"

-- | Direction of a transition
data PDFTransDirection2 = LeftToRight
                        | BottomToTop -- ^ Wipe only
                        | RightToLeft -- ^ Wipe only
                        | TopToBottom
                        | TopLeftToBottomRight -- ^ Glitter only
                        deriving(Eq)

-- | The PDF Monad
newtype PDF a = PDF {unPDF :: State PdfState a}
#ifndef __HADDOCK__
  deriving (Functor, Applicative, Monad, MonadState PdfState)
#else
instance Functor PDF
instance Monad PDF
instance MonadState PdfState PDF
#endif

-- | Transition style
data PDFTransStyle = Split PDFTransDimension PDFTransDirection
                   | Blinds PDFTransDimension 
                   | Box  PDFTransDirection
                   | Wipe PDFTransDirection2
                   | Dissolve 
                   | Glitter PDFTransDirection2
                   deriving(Eq)

-- | Document metadata
data PDFDocumentInfo = PDFDocumentInfo {
                     author :: PDFString
                   , subject :: PDFString
                   , pageMode :: PDFDocumentPageMode
                   , pageLayout :: PDFDocumentPageLayout
                   , viewerPreferences :: PDFViewerPreferences
                   , compressed :: Bool
                   }


-- | Document page mode
data PDFDocumentPageMode = UseNone
                       | UseOutlines
                       | UseThumbs
                       | FullScreen
                       deriving(Eq,Show)

-- | Document page layout
data PDFDocumentPageLayout = SinglePage
                           | OneColumn
                           | TwoColumnLeft
                           | TwoColumnRight
                           | TwoPageLeft
                           | TwoPageRight
                           deriving(Eq,Show)

-- | Viewer preferences
data PDFViewerPreferences = PDFViewerPreferences { hideToolbar :: Bool -- ^ To hide the toolbar
                          , hideMenuBar :: Bool -- ^ To hide the menubar
                          , hideWindowUI :: Bool -- ^ To hide the window
                          , fitWindow :: Bool -- ^ Fit window to screen
                          , centerWindow :: Bool -- ^ Center window on screen
                          , displayDoctitle :: Bool -- ^ Display the docu,ent title
                          , nonFullScreenPageMode :: PDFDocumentPageMode -- ^ Display mode when exiting the full screen mode
                          }

data PDFOutline = PDFOutline !(PDFReference PDFOutlineEntry) !(PDFReference PDFOutlineEntry)

instance PdfObject PDFOutline where
 toPDF (PDFOutline first lasto) = toPDF $ PDFDictionary. M.fromList $ [
    (PDFName "Type",AnyPdfObject . PDFName $ "Outlines")
  , (PDFName "First",AnyPdfObject first)
  , (PDFName "Last",AnyPdfObject lasto)
  ]

instance PdfLengthInfo PDFOutline where

data OutlineStyle = NormalOutline
                  | ItalicOutline
                  | BoldOutline
                  deriving(Eq)

data PDFOutlineEntry = PDFOutlineEntry !PDFString 
                              !(PDFReference PDFOutlineEntry) -- Parent
                              !(Maybe (PDFReference PDFOutlineEntry)) -- Prev
                              !(Maybe (PDFReference PDFOutlineEntry)) -- Next
                              !(Maybe (PDFReference PDFOutlineEntry)) -- First
                              !(Maybe (PDFReference PDFOutlineEntry)) -- Last
                              Int -- Count of descendent (negative)
                              Destination
                              Color --
                              OutlineStyle 

data Destination = Destination !(PDFReference PDFPage) deriving(Eq,Show)

-- Outline types without a position pointer. The true outline is the derivative
type OutlineData = (PDFString,Maybe Color, Maybe OutlineStyle,Destination)
type Outline = OutlineLoc OutlineData

data Tree a = Node a [Tree a]

data OutlineCtx a = Top | Child { value :: a
                                , parent :: OutlineCtx a 
                                , lefts :: [Tree a]
                                , rights :: [Tree a]
                                }
                                

data OutlineLoc  a = OutlineLoc (Tree a) (OutlineCtx a)

instance PdfObject PDFViewerPreferences where
  toPDF (PDFViewerPreferences ht hm hwui fw cw ddt nfspm ) = toPDF $ PDFDictionary. M.fromList $ 
   [ (PDFName "HideToolbar",AnyPdfObject ht)
   , (PDFName "HideMenubar",AnyPdfObject hm)
   , (PDFName "HideWindowUI",AnyPdfObject hwui)
   , (PDFName "FitWindow",AnyPdfObject fw)
   , (PDFName "CenterWindow",AnyPdfObject cw)
   , (PDFName "DisplayDocTitle",AnyPdfObject ddt)
   , (PDFName "NonFullScreenPageMode",AnyPdfObject  . PDFName . show $ nfspm)
   ]

instance PdfLengthInfo PDFViewerPreferences where


instance Show PDFTransStyle where
   show (Split _ _) = "Split"
   show (Blinds _) = "Blinds"
   show (Box _) = "Box"
   show (Wipe _) = "Wipe"
   show (Dissolve) = "Dissolve"
   show (Glitter _) = "Glitter"

instance PdfObject PDFTransition where
 toPDF (PDFTransition d t) = toPDF $ PDFDictionary. M.fromList $ 
   [ (PDFName "Type",AnyPdfObject (PDFName "Trans"))
   , (PDFName "S",AnyPdfObject (PDFName (show t)))
   , (PDFName "D",AnyPdfObject d)
   ] ++ optionalDm t ++ optionalM t ++ optionalDi t
  where
    optionalDm (Split a _) = [ (PDFName "Dm",AnyPdfObject (PDFName (show a)))]
    optionalDm (Blinds a) = [ (PDFName "Dm",AnyPdfObject (PDFName (show a)))]
    optionalDm _ = []
    optionalM (Split _ a) = [ (PDFName "M",AnyPdfObject (PDFName (show a)))]
    optionalM (Box a) = [ (PDFName "M",AnyPdfObject (PDFName (show a)))]
    optionalM _ = []    
    optionalDi (Wipe a) = [ (PDFName "Di",AnyPdfObject (floatDirection a))]
    optionalDi (Glitter a)  = [ (PDFName "Di",AnyPdfObject (floatDirection a))]
    optionalDi _ = []  

instance PdfLengthInfo PDFTransition where

-- PDF Pages

instance PdfObject PDFPages where
 toPDF (PDFPages c Nothing l) = toPDF $ PDFDictionary. M.fromList $ 
  [ (PDFName "Type",AnyPdfObject (PDFName "Pages"))
  , (PDFName "Kids",AnyPdfObject $ map AnyPdfObject l)
  , (PDFName "Count",AnyPdfObject . PDFInteger $ c)
  ] 
 toPDF (PDFPages c (Just theParent) l) = toPDF $ PDFDictionary. M.fromList $ 
  [ (PDFName "Type",AnyPdfObject (PDFName "Pages"))
  , (PDFName "Parent",AnyPdfObject theParent)
  , (PDFName "Kids",AnyPdfObject $ map AnyPdfObject l)
  , (PDFName "Count",AnyPdfObject . PDFInteger $ c)
  ] 

instance PdfLengthInfo PDFPages where


instance PdfObject PDFPage where
 toPDF (PDFPage (Just theParent) box content theRsrc d t theAnnots) = toPDF $ PDFDictionary. M.fromList $ 
  [ (PDFName "Type",AnyPdfObject (PDFName "Page"))
  , (PDFName "Parent",AnyPdfObject theParent)
  , (PDFName "MediaBox",AnyPdfObject box)
  , (PDFName "Contents",AnyPdfObject content)
  , if isJust theRsrc 
      then
       (PDFName "Resources",AnyPdfObject . fromJust $ theRsrc) 
      else 
       (PDFName "Resources",AnyPdfObject emptyDictionary)
  ] ++ (maybe [] (\x -> [(PDFName "Dur",AnyPdfObject x)]) d)
  ++ (maybe [] (\x -> [(PDFName "Trans",AnyPdfObject x)]) t)
  ++ ((\x -> if null x then [] else [(PDFName "Annots",AnyPdfObject x)]) theAnnots)
 toPDF (PDFPage Nothing _ _ _ _ _ _) = noPdfObject

instance PdfLengthInfo PDFPage where

-- Main objects in a PDF document

instance PdfObject PDFCatalog where
 toPDF (PDFCatalog outlines lPages pgMode pgLayout viewerPrefs) = toPDF $ PDFDictionary . M.fromList $ 
   [ (PDFName "Type",AnyPdfObject (PDFName "Catalog"))
   , (PDFName "Pages",AnyPdfObject lPages)
   , (PDFName "PageMode", AnyPdfObject . PDFName . show $ pgMode)
   , (PDFName "PageLayout", AnyPdfObject . PDFName . show $ pgLayout)
   , (PDFName "ViewerPreferences", AnyPdfObject viewerPrefs)
   ] ++ (maybe [] (\x -> [(PDFName "Outlines",AnyPdfObject x)]) outlines)

instance PdfLengthInfo PDFCatalog where

instance PdfObject OutlineStyle where
   toPDF NormalOutline = toPDF (PDFInteger 0)
   toPDF ItalicOutline = toPDF (PDFInteger 1)
   toPDF BoldOutline = toPDF (PDFInteger 2)

instance PdfLengthInfo OutlineStyle where

instance PdfObject PDFOutlineEntry where
 toPDF (PDFOutlineEntry title theParent prev next first theLast count dest color style) = 
     toPDF $ PDFDictionary. M.fromList $ [
        (PDFName "Title",AnyPdfObject title)
        , (PDFName "Parent",AnyPdfObject theParent)
        ]
      ++
      maybe [] (\x -> [(PDFName "Prev",AnyPdfObject x)]) prev
      ++
      maybe [] (\x -> [(PDFName "Next",AnyPdfObject x)]) next
      ++
      maybe [] (\x -> [(PDFName "First",AnyPdfObject x)]) first
      ++
      maybe [] (\x -> [(PDFName "Last",AnyPdfObject x)]) theLast
      ++
      [ (PDFName "Count",AnyPdfObject (PDFInteger count))
      , (PDFName "Dest",AnyPdfObject dest)
      , (PDFName "C",AnyPdfObject color)
      , (PDFName "F",AnyPdfObject style)
      ]

instance PdfLengthInfo PDFOutlineEntry where


instance PdfObject Destination where
  toPDF (Destination r) = toPDF                [ AnyPdfObject r
                                               , AnyPdfObject . PDFName $ "Fit"
                                               ]

instance PdfLengthInfo Destination where

                                              
instance PdfObject Color where
   toPDF (Rgb r g b) = toPDF . map AnyPdfObject $ [r,g,b]  
   toPDF (Hsv h s v) = let (r,g,b) = hsvToRgb (h,s,v)
    in toPDF . map AnyPdfObject $ [r,g,b]

instance PdfLengthInfo Color where

-- Degree for a transition direction
floatDirection :: PDFTransDirection2 -> PDFFloat
floatDirection LeftToRight = 0
floatDirection BottomToTop = 90
floatDirection RightToLeft = 180 
floatDirection TopToBottom = 270
floatDirection TopLeftToBottomRight = 315


hsvToRgb :: (Double,Double,Double) -> (Double,Double,Double)
hsvToRgb (h,s,v) =
  let hi = fromIntegral (floor (h / 60) `mod` 6 :: Int) :: Double
      f = h/60 - hi
      p = v * (1-s)
      q = v * (1 - f*s)
      t = v * (1 - (1-f)*s) in
 case hi of
      0 -> (v,t,p)
      1 -> (q,v,p)
      2 -> (p,v,t)
      3 -> (p,q,v)
      4 -> (t,p,v)
      5 -> (v,p,q)
      _ -> error "Hue value incorrect"

getRgbColor :: Color -> (PDFFloat,PDFFloat,PDFFloat) 
getRgbColor (Rgb r g b) = (r, g, b)  
getRgbColor (Hsv h s v) = let (r,g,b) = hsvToRgb (h,s,v) in (r, g, b)  

-- | Interpolation function
interpole :: Int -> PDFFloat -> PDFFloat -> AnyPdfObject
interpole n x y = AnyPdfObject . PDFDictionary . M.fromList $ 
                            [ (PDFName "FunctionType", AnyPdfObject . PDFInteger $ 2)
                            , (PDFName "Domain", AnyPdfObject . map AnyPdfObject $ ([0,1] :: [PDFFloat]))
                            , (PDFName "C0", AnyPdfObject . map AnyPdfObject $ [x])
                            , (PDFName "C1", AnyPdfObject . map AnyPdfObject $ [y])
                            , (PDFName "N", AnyPdfObject . PDFInteger $  n)
                            ]

-- | A shading                             
data PDFShading = AxialShading PDFFloat PDFFloat PDFFloat PDFFloat Color Color
                | RadialShading PDFFloat PDFFloat PDFFloat PDFFloat PDFFloat PDFFloat Color Color
                deriving(Eq,Ord)

instance PdfResourceObject PDFShading where
      toRsrc (AxialShading x0 y0 x1 y1 ca cb) = AnyPdfObject . PDFDictionary . M.fromList $
                                 [ (PDFName "ShadingType",AnyPdfObject . PDFInteger $ 2)
                                 , (PDFName "Coords",AnyPdfObject . map AnyPdfObject $ [x0,y0,x1,y1])
                                 , (PDFName "ColorSpace",AnyPdfObject . PDFName $ "DeviceRGB")
                                 , (PDFName "Function",AnyPdfObject $ [interpole 1 ra rb,interpole 1 ga gb,interpole 1 ba bb])
                                 ]
        where
            (ra,ga,ba) = getRgbColor ca
            (rb,gb,bb) = getRgbColor cb
      toRsrc (RadialShading x0 y0 r0 x1 y1 r1 ca cb) = AnyPdfObject . PDFDictionary . M.fromList $
                                         [ (PDFName "ShadingType",AnyPdfObject . PDFInteger $ 3)
                                         , (PDFName "Coords",AnyPdfObject . map AnyPdfObject $ [x0,y0,r0,x1,y1,r1])
                                         , (PDFName "ColorSpace",AnyPdfObject . PDFName $ "DeviceRGB")
                                         , (PDFName "Function",AnyPdfObject $ [interpole 1 ra rb,interpole 1 ga gb,interpole 1 ba bb])
                                         ]
        where
           (ra,ga,ba) = getRgbColor ca
           (rb,gb,bb) = getRgbColor cb


-- | Apply a transformation matrix to the current coordinate frame
applyMatrix :: Matrix -> Draw ()
applyMatrix m@(Matrix a b c d e f)  = do
    multiplyCurrentMatrixWith m
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
                    , serialize " cm"
                    ]
