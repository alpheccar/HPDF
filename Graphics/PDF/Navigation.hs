---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Navigation
---------------------------------------------------------

module Graphics.PDF.Navigation(
  -- * Navigation
  -- ** Types
    OutlineStyle(..)
  -- ** Functions
  , newSection
  , newSectionWithPage
 ) where
     
import Graphics.PDF.Pages
import Graphics.PDF.Draw
import Graphics.PDF.LowLevel.Types
import Control.Monad.State(gets)
import Control.Monad(when)
import Data.Maybe(isNothing)

-- | True if we are adding the first outline to this level
isFirst :: [Bool] -> Bool
isFirst r = head r
  
-- | Start a new outline level  
startNew :: PDF ()
startNew = modifyStrict $ \s -> s{firstOutline = True:(firstOutline s)}

-- | We remember there are outlines at this level
addedOutline :: PDF ()
addedOutline = modifyStrict $ \s -> s{firstOutline = False:tail (firstOutline s)}

-- | Close an outline level
closeNew :: PDF()
closeNew = do
    r <- gets firstOutline
    when (not (isFirst r)) $ moveToParent
    modifyStrict $ \s -> s{firstOutline = tail (firstOutline s)}

-- | Create a new outline section pointing to the last created page
newSection :: PDFString -- ^ Outline title
           -> Maybe Color -- ^ Outline color
           -> Maybe OutlineStyle -- ^Outline style
           -> PDF ()
           -> PDF ()
newSection myS col style p = newSectionPrivate myS col style Nothing p

-- | Create a new outline section pointing to a given page
newSectionWithPage :: PDFString -- ^ Outline title
                   -> Maybe Color -- ^ Outline color
                   -> Maybe OutlineStyle -- ^ Outline style
                   -> PDFReference PDFPage -- ^ Page reference
                   -> PDF ()
                   -> PDF ()
newSectionWithPage myS col style page p = newSectionPrivate myS col style (Just page) p
    
newSectionPrivate :: PDFString -- ^ Outline title
                  -> Maybe Color -- ^ Outline color
                  -> Maybe OutlineStyle -- ^Outline style
                  -> Maybe (PDFReference PDFPage)
                  -> PDF ()
                  -> PDF ()
newSectionPrivate myS col style page p = do
       let newlevel = do
            startNew
            p
            closeNew
       r <- gets firstOutline
       if isFirst r
        then do
            if length r > 1 
             then do
                newChild myS col style page
                addedOutline
                newlevel
             else do
                newSibling myS col style page
                newlevel
        else do
           newSibling myS col style page
           newlevel                  
                   
newSibling :: PDFString -- ^ Outline title
           -> Maybe Color -- ^ Outline color
           -> Maybe OutlineStyle -- ^Outline style
           -> Maybe (PDFReference PDFPage)
           -> PDF ()
newSibling myS col style page = do
    p <- if isNothing page then gets currentPage else return page
    case p of
        Nothing -> return ()
        Just aPage -> do
            ot <- gets outline
            let myValue = (myS,col,style,Destination aPage)
            case ot of
                Nothing -> modifyStrict $ \s -> s {outline = Just $ insertDown myValue (OutlineLoc (Node myValue []) Top)}
                Just r -> modifyStrict $ \s -> s {outline = Just $ insertRight myValue r}
                
newChild :: PDFString -- ^ Outline title
         -> Maybe Color -- ^ Outline color
         -> Maybe OutlineStyle -- ^Outline style
         -> Maybe (PDFReference PDFPage)
         -> PDF ()
newChild myS col style page = do
    p <- if isNothing page then gets currentPage else return page
    case p of
        Nothing -> return ()
        Just aPage -> do
            ot <- gets outline
            let myValue = (myS,col,style,Destination aPage)
            case ot of
                Nothing -> modifyStrict $ \s -> s {outline = Just $ insertDown myValue (OutlineLoc (Node myValue []) Top)}
                Just r -> modifyStrict $ \s -> s {outline = Just $ insertDown myValue r}
                
moveToParent :: PDF ()
moveToParent = do
    ot <- gets outline
    case ot of
       Nothing -> return ()
       Just r -> modifyStrict $ \s -> s {outline = Just $ up r}
