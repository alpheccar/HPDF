{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Resources
---------------------------------------------------------
-- #hide
module Graphics.PDF.Resources(
   PDFResource(..)
 , addResource
 , emptyRsrc
 , StrokeAlpha(..)
 , FillAlpha(..)
 , PdfResourceObject(..)
 , resourceToDict
 , emptyResource
 , PDFColoredPattern
 , PDFUncoloredPattern
 , AnyPdfPattern
 , PDFColorSpace(..)
 ) where
     
import Graphics.PDF.LowLevel.Types
import qualified Data.Map as M



      
newtype StrokeAlpha = StrokeAlpha Double deriving(Eq,Ord)  
instance PdfResourceObject StrokeAlpha where
  toRsrc (StrokeAlpha a) = AnyPdfObject . PDFDictionary . M.fromList $ [(PDFName "CA",AnyPdfObject a)]
  
newtype FillAlpha = FillAlpha Double deriving(Eq,Ord)  
instance PdfResourceObject FillAlpha where
  toRsrc (FillAlpha a) = AnyPdfObject . PDFDictionary . M.fromList $ [(PDFName "ca",AnyPdfObject a)]
  
class PdfResourceObject a where
      toRsrc :: a -> AnyPdfObject
      
        
-- | A PDF Resource
data PDFResource = PDFResource  {
                   procSet :: !PDFArray
                 , resources :: M.Map PDFName PDFDictionary
                 }


emptyRsrc :: PDFResource              
--emptyRsrc = PDFResource [AnyPdfObject . PDFName $ "PDF"] (M.empty)
emptyRsrc = PDFResource [] (M.empty)        

getResources :: M.Map PDFName PDFDictionary -> [(PDFName,AnyPdfObject)]
getResources = M.toList . M.map AnyPdfObject

instance PdfObject PDFResource where
 toPDF r = toPDF . resourceToDict $ r
     
instance PdfLengthInfo PDFResource where

-- | Add a new G State to the G State dictionary for the given resource
addResource :: PDFName -- ^ GState dictionary
          -> PDFName -- ^ GState name must be unique
          -> AnyPdfObject -- ^ G State content
          -> PDFResource -- ^ Old resource
          -> PDFResource -- ^ New resource
addResource dict name newValue r = let addValue (Just (PDFDictionary a)) = Just . PDFDictionary $ M.insert name newValue a
                                       addValue (Nothing) = Just . PDFDictionary $ M.insert name newValue M.empty
 in
  r {resources = M.alter addValue dict (resources r)}
    
-- | Convert the resource to a PDf dictionary
resourceToDict :: PDFResource -> PDFDictionary
resourceToDict r = PDFDictionary . M.fromList  $
    --[(PDFName "ProcSet",AnyPdfObject (procSet r))] ++
    getResources (resources r)
    
emptyResource :: PDFResource -> Bool
emptyResource (PDFResource a b) = null a && M.null b 


-- | A PDF Pattern
data PDFUncoloredPattern
data PDFColoredPattern
data AnyPdfPattern


-- | A PDF Color space
data PDFColorSpace = PatternRGB  deriving(Eq,Ord)

instance PdfResourceObject PDFColorSpace where
    toRsrc PatternRGB = AnyPdfObject . map AnyPdfObject $ [PDFName "Pattern",PDFName "DeviceRGB"]
    
instance PdfObject PDFColoredPattern where
    toPDF _ = noPdfObject
instance PdfLengthInfo PDFColoredPattern where

instance PdfResourceObject (PDFReference PDFColoredPattern) where
    toRsrc = AnyPdfObject

instance PdfObject PDFUncoloredPattern where
        toPDF _ = noPdfObject
instance PdfLengthInfo PDFUncoloredPattern where

instance PdfResourceObject (PDFReference PDFUncoloredPattern) where
        toRsrc = AnyPdfObject

instance PdfObject AnyPdfPattern where
        toPDF _ = noPdfObject
instance PdfLengthInfo AnyPdfPattern where

instance PdfResourceObject (PDFReference AnyPdfPattern) where
        toRsrc = AnyPdfObject

  
