---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Actions
---------------------------------------------------------

module Graphics.PDF.Action(
   -- * Actions
   -- ** Types
     Action
   , GoToURL(..)
   -- ** Functions
 ) where
     
import Graphics.PDF.LowLevel.Types
import qualified Data.Map as M


--  Media action
--data MediaAction = Play
--                 | Stop
--                 | Pause
--                 | Resume
--                 deriving(Enum)

class PdfObject a => Action a

-- | Action of going to an URL
newtype GoToURL = GoToURL String

--data Rendition = Rendition
--instance PdfObject Rendition where
--  toPDF a = toPDF . PDFDictionary . M.fromList $
--                    [ (PDFName "Type",AnyPdfObject . PDFName $ "Rendition")
--                    , (PDFName "S",AnyPdfObject . PDFName $ "MR")
--                    , (PDFName "C",AnyPdfObject movie)
--                    ]
--    where
--        movie = PDFDictionary . M.fromList $
--               [ (PDFName "Type",AnyPdfObject . PDFName $ "MediaClip")
--               , (PDFName "S",AnyPdfObject . PDFName $ "MCD")
--               , (PDFName "CT",AnyPdfObject . toPDFString $ "video/3gpp")
--               , (PDFName "D",AnyPdfObject (toPDFString "17.3gp"))
--               ]

--  Action to control a media
--data ControlMedia = ControlMedia MediaAction Int (PDFReference Rendition)
    
instance PdfObject GoToURL where
    toPDF (GoToURL s) = toPDF . PDFDictionary . M.fromList $
                         [ (PDFName "Type",AnyPdfObject . PDFName $ "Action")
                         , (PDFName "S",AnyPdfObject (PDFName "URI"))
                         , (PDFName "URI",AnyPdfObject (toPDFString s))
                         ]
instance Action GoToURL

instance PdfLengthInfo GoToURL where


--instance PdfObject ControlMedia where
--    toPDF (ControlMedia operation relatedScreenAnnotation rendition) = toPDF . PDFDictionary . M.fromList $
--                         [ (PDFName "Type",AnyPdfObject . PDFName $ "Action")
--                         , (PDFName "S",AnyPdfObject (PDFName "Rendition"))
--                         , (PDFName "R",AnyPdfObject rendition)
--                         , (PDFName "OP",AnyPdfObject . PDFInteger $ (fromEnum operation))
--                         , (PDFName "AN",AnyPdfObject $ (PDFReference relatedScreenAnnotation :: PDFReference AnyPdfObject))
--                         ]
--                         
--instance Action ControlMedia
