---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Low level page management
---------------------------------------------------------
-- #hide
module Graphics.PDF.Pages(
 -- * Low level stuff
 -- ** Document management
   standardViewerPrefs
 -- ** Page management
 , findPage
 , recordPage
 , noPages
 , addPages
 , getCurrentPage
 -- ** PDF Object management
 , addObject
 , supply
 , updateObject
 , addOutlines
 , insertDown
 , insertRight
 , up
 , createContent
 , recordBound
 , setPageResource
 , setPageAnnotations
 ) where
     
import qualified Data.IntMap as IM
import Control.Monad.State
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import qualified Graphics.PDF.Data.PDFTree as PT hiding(PDFTree,Key)
import Graphics.PDF.Resources
import Data.List(zip4)

import Graphics.PDF.Data.PDFTree(PDFTree,Key)

-- | Set page annotations
setPageAnnotations :: [AnyAnnotation] -> PDFReference PDFPage -> PDF ()
setPageAnnotations an page = do
    -- Get the page dictionary
    lPages <- gets pages
    -- Look for the page
    let thePage = findPage page lPages
    case thePage of
       Nothing -> return ()
       -- If the page is found, get its stream reference and look for the stream
       Just (PDFPage a b c d e f _) -> do
           refs <- mapM (\x -> addAnnotation x >>= return . AnyPdfObject) an
           modifyStrict $ \s -> s {pages = recordPage page (PDFPage a b c d e f refs) lPages}
           
-- | Set page resource
setPageResource :: PDFReference PDFResource -> PDFReference PDFPage -> PDF ()
setPageResource newr page = do
    -- Get the page dictionary
    lPages <- gets pages
    -- Look for the page
    let thePage = findPage page lPages
    case thePage of
        Nothing -> return ()
        -- If the page is found, get its stream reference and look for the stream
        Just (PDFPage a b c _ e f g) -> modifyStrict $ \s -> s {pages = recordPage page (PDFPage a b c (Just newr) e f g) lPages}


-- | Create a new empty content for a page
createContent :: Draw a -- ^ List of drawing commands
              -> Maybe (PDFReference PDFPage)
              -> PDF (PDFReference PDFStream) -- ^ Reference to the drawing
createContent d page = do
  -- Create a new stream referenbce
  streamref <- supply
  myBounds <- gets xobjectBound
  let (_,state',w') = runDrawing d (emptyEnvironment {streamId = streamref, xobjectBoundD = myBounds}) (emptyDrawState streamref)
  modifyStrict $ \s -> s {streams = IM.insert streamref (page,(state',w')) (streams s)}
  return (PDFReference streamref)

-- | Returns a new unique identifier
supply :: PDF Int
supply = do
          r <- gets supplySrc
          modifyStrict $ \s -> s {supplySrc = r+1}
          return r
     
-- | Add an object to the PDF object dictionary and return a PDF reference     
addObject :: (PdfObject a, PdfLengthInfo a) => a -> PDF (PDFReference a)
addObject a = do
  r <- supply
  modifyStrict $ \s -> s {objects = IM.insert r (AnyPdfObject a) (objects s)}
  return (PDFReference r)
  

-- | Update a referenced object with a new one
updateObject :: (PdfObject a, PdfLengthInfo a) => PDFReference a -- ^ Reference to the initial object
             -> a -- ^ New value
             -> PDF ()
updateObject (PDFReference i) obj = do
   modifyStrict $ \s -> s {objects = IM.insert i (AnyPdfObject obj) (objects s)}


                          

                  
standardViewerPrefs :: PDFViewerPreferences
standardViewerPrefs = PDFViewerPreferences False False False False False False UseNone


                        




-- | Record the page in the page catalog
recordPage :: PDFReference  PDFPage -- ^ Reference to the page
           -> PDFPage -- ^ Page content
           -> Pages -- ^ Pages n the documents
           -> Pages
recordPage pageref page (Pages lPages) = Pages (PT.insert pageref page lPages)

-- | Find a page in the catalog
findPage :: PDFReference PDFPage -- ^ Reference to the page
         -> Pages -- ^ Pages in the document
         -> Maybe PDFPage -- ^ Page content if found
findPage page (Pages lPages) = PT.lookup page lPages

-- | Add a node PDFTree object
nodePage :: Maybe (PDFReference PDFPages) -- ^ Parent node
         -> PDFTree PDFPage -- ^ Left tree
         -> PDFTree PDFPage -- ^ Right tree
         -> PDF (Int,PDFReference PDFPages) -- ^ PDF reference to the new node pointing to the left and right ones
nodePage ref l r = do
    n <- supply
    -- Reserve an identifier for the root page object
    let pRef = (PDFReference n) :: PDFReference PDFPages
    (sl,lr) <- PT.fold2 (Just pRef) nodePage leafPage l
    (sr,rr) <- PT.fold2 (Just pRef) nodePage leafPage r
    let len = sl + sr
    case (PT.isLeaf l,PT.isLeaf r) of
        (False,False) -> updateObject pRef $ PDFPages len ref [Left lr,Left rr]
        (True,False) -> updateObject pRef $ PDFPages len ref [Right (PT.keyOf l),Left rr]
        (False,True) -> updateObject pRef $ PDFPages len ref [Left lr,Right (PT.keyOf r)]
        (True,True) -> updateObject pRef $ PDFPages len ref [Right (PT.keyOf l),Right (PT.keyOf r)]
    return (len,pRef)
        
  
-- | Add a page to the PDG object dictionary
leafPage :: Maybe (PDFReference PDFPages) -- ^ Page parent if any
         -> Key PDFPage -- ^ Page reference
         -> PDFPage -- ^ Page data
         -> PDF (Int,PDFReference PDFPages)  -- ^ Reference to a PDFPages objects
leafPage (Just ref) (PDFReference objectnb) (PDFPage _ a b c d e f) = do
      modifyStrict $ \s -> s {objects = IM.insert objectnb (AnyPdfObject $ PDFPage (Just ref) a b c d e f) (objects s) }
      return (1,ref)
      
leafPage Nothing p@(PDFReference objectnb) (PDFPage _ a b c d e f) = do
     n <- supply
     -- Reserve an identifier for the root page object
     let pRef = (PDFReference n) :: PDFReference PDFPages
     updateObject pRef $ PDFPages 1 Nothing [Right p]
     modifyStrict $ \s -> s {objects = IM.insert objectnb (AnyPdfObject $ PDFPage (Just pRef) a b c d e f) (objects s) }
     return (1,pRef)   
                  
-- | Add all pages to the PDF object dictionary
addPages :: PDF (PDFReference PDFPages)
addPages = do
    Pages lPages <- gets pages
    (_,r) <- PT.fold2 Nothing nodePage leafPage lPages
    return r
    
-- | Empty page catalog
noPages :: Pages
noPages = Pages (PT.empty)
      

-- insert a subtree to the right of the current node
insertRight :: a -> OutlineLoc a -> OutlineLoc a
insertRight _ (OutlineLoc _ Top) = error "Cannot insert right of the top node"
insertRight t' (OutlineLoc t c  ) = let c' = Child { value = value c
                                                   , parent = parent c
                                                   , rights = rights c
                                                   , lefts  = lefts c ++ [t] }
                               in OutlineLoc (Node t' []) c'
                              
insertDown :: a -> OutlineLoc a -> OutlineLoc a
insertDown t' (OutlineLoc (Node v cs) c) = let c' = Child { value = v
                                                          , parent = c
                                                          , rights = []
                                                          , lefts  = cs 
                                                          }
                                in  OutlineLoc (Node t' []) c'
                                
-- move up
up :: OutlineLoc a -> OutlineLoc a
up (OutlineLoc _ Top            ) = error "Cannot go up from the top node"
up (OutlineLoc t (Child v c ls rs)) = let t' = Node v  (ls ++ [t] ++ rs)
                                  in OutlineLoc t' c
                              

addOutlines :: Maybe Outline -> PDF (Maybe (PDFReference PDFOutline))
addOutlines Nothing = return Nothing
addOutlines (Just r) = do
  let (Node _ l) = toTree r
  if null l
    then return Nothing
    else do
      rootRef <- supply
      (first,end) <- createOutline (PDFReference rootRef) l
      let outlineCatalog = PDFOutline first end
      updateObject (PDFReference rootRef) outlineCatalog
      return (Just (PDFReference rootRef))
      
 
createOutline :: PDFReference PDFOutlineEntry -> [Tree OutlineData] -> PDF (PDFReference PDFOutlineEntry,PDFReference PDFOutlineEntry)
createOutline r children = do
    -- Get references for all these outlines
    refs' <- mapM (const (supply >>= return . Just . PDFReference)) children
    -- (previousRef, currentRef, currentNode, nextRef)
    let refs = zip4 (Nothing : init refs') refs' children (tail refs' ++ [Nothing])
        current (_,c,_,_) = c
        Just first = current (head refs)
        Just end = current (last refs)
    mapM_ (addEntry first end) refs
    return (first,end)
 where
    addEntry _ _ (_,Nothing,_,_) = error "This pattern match in addEntry should never occur !"
    addEntry _ _ (prev,Just current,Node (title,col,style,dest) c,next) = do
        (f,e) <- if (null c) 
            then 
                return (Nothing,Nothing) 
            else
                createOutline current c >>= \(x,y) -> return (Just x,Just y)
        let o = PDFOutlineEntry title
                                r -- Parent
                                prev -- Prev
                                next
                                f
                                e
                                (-(length c))
                                dest
                                (maybe (Rgb 0 0 0) id col)
                                (maybe NormalOutline id style)
        updateObject current o
    

toTree :: OutlineLoc a -> Tree a
toTree (OutlineLoc a Top) = a
toTree a = toTree (up a)


-- | Reference to the last created page
getCurrentPage :: PDF (Maybe (PDFReference PDFPage))
getCurrentPage = gets currentPage

-- | Record bound of an xobject
recordBound :: Int -- ^ Reference
            -> PDFFloat -- ^ Width
            -> PDFFloat -- ^ Height
            -> PDF ()
recordBound ref width height = modifyStrict $ \s -> s {xobjectBound = IM.insert ref (width,height) (xobjectBound s)}

