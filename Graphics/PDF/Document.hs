---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Management of the PDF structure
---------------------------------------------------------
{-# LANGUAGE CPP #-}
module Graphics.PDF.Document(
 -- * Document actions
 -- ** Special document objects
   PDFXForm
 -- ** Page management
 , addPage
 , addPageWithTransition
 , drawWithPage
 , createPDFXForm
 -- ** Page transitions
 , PDFTransition(..)
 , PDFTransStyle(..)
 , PDFTransDirection(..)
 , PDFTransDimension(..)
 , PDFTransDirection2(..)
 -- ** Document information
 , PDFDocumentInfo(..)
 , PDFDocumentPageMode(..)
 , PDFDocumentPageLayout(..)
 , PDFViewerPreferences(..)
 , standardDocInfo
 , standardViewerPrefs
 -- * Draw monad and drawing functions
 -- ** Types
 , Draw
 , PDFXObject(drawXObject)
 , PDFGlobals(..)
 -- ** General drawing functions
 , withNewContext
 , emptyDrawing
 ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Pages
import Control.Monad.State
import qualified Data.IntMap as IM
import qualified Data.Map as M
        
-- | No information for the document  
standardDocInfo :: PDFDocumentInfo          
standardDocInfo = PDFDocumentInfo (toPDFString "") (toPDFString "") UseNone SinglePage standardViewerPrefs True

-- | Create a PDF XObject
createPDFXForm :: PDFFloat -- ^ Left
              -> PDFFloat -- ^ Bottom
              -> PDFFloat -- ^ Right
              -> PDFFloat -- ^ Top
              -> Draw a -- ^ Drawing commands
              -> PDF (PDFReference PDFXForm)
createPDFXForm xa ya xb yb d = let a' = do modifyStrict $ \s -> s  {otherRsrcs = PDFDictionary. M.fromList $ 
                                                                       [ (PDFName "Type",AnyPdfObject . PDFName $ "XObject")
                                                                       , (PDFName "Subtype",AnyPdfObject . PDFName $ "Form")
                                                                       , (PDFName "FormType",AnyPdfObject . PDFInteger $ 1)
                                                                       , (PDFName "Matrix",AnyPdfObject . (map (AnyPdfObject . PDFInteger)) $ [1,0,0,1,0,0])
                                                                       , (PDFName "BBox",AnyPdfObject . (map AnyPdfObject) $ [xa,ya,xb,yb])
                                                                       ]
                                                                  }
                                           d
 in do
     PDFReference s <- createContent a' Nothing  
     recordBound s (xb-xa) (yb-ya)
     return (PDFReference s)       

 
-- Create a new empty page
createANewPage :: Maybe PDFRect -- ^ Page size or default document's one
               -> PDF (Int,PDFPage) -- ^ Reference to the new page
createANewPage rect' = do
       rect <- maybe (gets defaultRect) return rect'
       -- Get the root page reference
       -- Create a new page reference
       pageref <- supply
       -- Create a new empty content for the page
       pageContent <- createContent (return ()) (Just (PDFReference pageref :: PDFReference PDFPage))
       -- Create a new page having as parent the root page
       let page = PDFPage Nothing rect pageContent Nothing Nothing Nothing []
       return (pageref , page)
       
-- | Add a new page to a PDF document
addPage :: Maybe PDFRect -- ^ Page size or default document's one
        -> PDF (PDFReference PDFPage) -- ^ Reference to the new page
addPage rect'   = do
   (pf,page) <- createANewPage rect'
   let pageref = PDFReference pf
   modifyStrict $ \s -> s {pages = recordPage pageref page (pages s), currentPage = Just pageref}
   return pageref
   
addPageWithTransition :: Maybe PDFRect -- ^ Page size or default document's one
                      -> Maybe PDFFloat -- ^ Optional duration
                      -> Maybe PDFTransition -- ^ Optional transition
                      -> PDF (PDFReference PDFPage) -- ^ Reference to the new page
addPageWithTransition rect' dur t = do
    (pf,PDFPage a b c d _ _ pageAnnots) <- createANewPage rect'
    let pageref = PDFReference pf
    modifyStrict $ \s -> s {pages = recordPage pageref (PDFPage a b c d dur t pageAnnots) (pages s), currentPage = Just pageref}
    return pageref

        
-- | Draw on a given page
drawWithPage :: PDFReference PDFPage -- ^ Page
            -> Draw a -- ^ Drawing commands
            -> PDF a
drawWithPage page draw = do
     -- Get the page dictionary
     lPages <- gets pages
     -- Get the stream dictionary
     lStreams <- gets streams
     -- Look for the page
     let thePage = findPage page lPages
     case thePage of
       Nothing -> error "Can't find the page to draw on it"
       -- If the page is found, get its stream reference and look for the stream
       Just(PDFPage _ _ (PDFReference streamRef) _ _ _ _) -> do
          let theContent = IM.lookup streamRef lStreams
          case theContent of
            Nothing -> error "Can't find a content for the page to draw on it"
            -- If the stream is found
            Just (_,(oldState,oldW)) -> do
              -- Create a new cntent and update the stream
              myBounds <- gets xobjectBound
              let (a,state',w') = runDrawing draw (emptyEnvironment {streamId = streamRef, xobjectBoundD = myBounds}) oldState
              modifyStrict $ \s -> s {streams = IM.insert streamRef (Just page,(state',mappend oldW w')) lStreams}
              return a
