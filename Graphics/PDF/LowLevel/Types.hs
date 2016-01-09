{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Low level stuff
---------------------------------------------------------
-- #hide
module Graphics.PDF.LowLevel.Types where

import qualified Data.Map.Strict as M
import Data.List(intersperse)
import Data.Int
import Control.Monad.State
import Control.Monad.Writer
import Data.Binary.Builder(Builder,fromByteString)
import Graphics.PDF.LowLevel.Serializer
import Data.Complex
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Internal as L(ByteString(..))
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.Word 
import Data.Char(ord)
import Text.Printf(printf)

{-

Low level typesetting types

-}
data SpecialChar = NormalChar !Char
                 | BreakingHyphen
                 | BiggerSpace
                 | NormalSpace

{-

PDF Specific low level types

-}

-- | PDF Objects
class PdfObject a where
  toPDF :: a -> Builder

class PdfLengthInfo a where 
  pdfLengthInfo :: a -> Maybe (Int64 , PDFReference MaybeLength)
  pdfLengthInfo _ = Nothing

-- | Anonymous PDF object
data AnyPdfObject = forall a . (PdfObject a, PdfLengthInfo a) => AnyPdfObject !a

instance PdfObject AnyPdfObject where
 toPDF (AnyPdfObject a) = toPDF a

instance PdfLengthInfo AnyPdfObject where 
  pdfLengthInfo (AnyPdfObject a) = pdfLengthInfo a
 
-- | An integer in a PDF document
newtype PDFInteger = PDFInteger Int deriving(Eq,Show,Ord,Num)

-- | A length in a PDF document
newtype PDFLength = PDFLength Int64 deriving(Eq,Show,Ord,Num)

data MaybeLength = UnknownLength 
                 | KnownLength !PDFLength 

instance PdfObject MaybeLength where 
  toPDF (KnownLength a) = toPDF a 
  toPDF (UnknownLength) = error "Trying to process an unknown length during PDF generation"

instance PdfLengthInfo MaybeLength where

-- | A real number in a PDF document
type PDFFloat = Double 

instance PdfObject PDFInteger where
    toPDF (PDFInteger a) = serialize a

instance PdfLengthInfo PDFInteger where

instance PdfObject Int where
    toPDF a = serialize a

instance PdfLengthInfo Int where

          
instance PdfObject PDFLength where
    toPDF (PDFLength a) = serialize (show a)

instance PdfLengthInfo PDFLength where

    
instance PdfObject PDFFloat where
  toPDF a = serialize a

instance PdfLengthInfo PDFFloat where


instance PdfObject (Complex PDFFloat) where
  toPDF (x :+ y) = mconcat [ serialize x
                           , serialize ' '
                           , serialize y
                           ] 

instance PdfLengthInfo (Complex PDFFloat) where

  
instance PdfObject Bool where
  toPDF (True) = serialize ("true" :: String)
  toPDF (False) = serialize ("false" :: String)

instance PdfLengthInfo Bool where


-- | A PDFString containing a strict bytestring (serialied as UTF16BE)
newtype PDFString = PDFString S.ByteString deriving(Eq,Ord,Show)

-- | A list of glyph to be used in text operators
newtype PDFGlyph = PDFGlyph S.ByteString deriving(Eq,Ord,Show)

-- | A list of glyph to be used in text operators
newtype EscapedPDFGlyph = EscapedPDFGlyph S.ByteString deriving(Eq,Ord,Show)

-- | 7 bit encoded ASCII string
newtype AsciiString = AsciiString S.ByteString deriving(Eq,Ord,Show)

-- | 7 bit encoded ASCII string
newtype EscapedAsciiString = EscapedAsciiString S.ByteString deriving(Eq,Ord,Show)

escapeText :: Char -> T.Text
escapeText '(' = "\\("
escapeText ')' = "\\)"
escapeText '\\' = "\\\\"
escapeText a = T.singleton a

escapeByteString :: Char -> S.ByteString
escapeByteString '(' = C.pack "\\("
escapeByteString ')' = C.pack "\\)"
escapeByteString '\\' = C.pack "\\\\"
escapeByteString a = C.singleton a

-- | Create a PDF string from an Haskell one
toPDFString :: T.Text -> PDFString
toPDFString = PDFString . encodeUtf16BE

toPDFGlyph :: S.ByteString -> PDFGlyph
toPDFGlyph = PDFGlyph  -- . escapeString

toAsciiString :: String -> AsciiString 
toAsciiString s = AsciiString (C.pack s)

toHexaStream :: PDFString -> S.ByteString 
toHexaStream (PDFString x) = 
    let hexChar c = C.pack (printf "%02X" (ord c) :: String)
    in
    C.cons 'F' . C.cons 'E' . C.cons 'F' . C.cons 'F' . C.concatMap hexChar $ x

newtype GlyphCode = GlyphCode Word8 deriving(Eq,Ord,Show,Integral,Bounded,Enum,Real,Num)


instance SerializeValue L.ByteString PDFString where
  serialize (PDFString t) = L.Chunk t L.Empty

instance SerializeValue Builder PDFString where
  serialize (PDFString t) = fromByteString t

instance SerializeValue L.ByteString PDFGlyph where
  serialize (PDFGlyph t) = L.Chunk t L.Empty


instance SerializeValue Builder EscapedPDFGlyph where
  serialize (EscapedPDFGlyph t) = fromByteString t

instance SerializeValue L.ByteString AsciiString where
  serialize (AsciiString t) = L.Chunk t L.Empty

instance SerializeValue Builder EscapedAsciiString where
  serialize (EscapedAsciiString t) = fromByteString t

-- Misc strings useful to build bytestrings

lparen :: SerializeValue s Char => s
lparen = serialize '('

rparen :: SerializeValue s Char => s
rparen = serialize  ')'

lbracket :: SerializeValue s Char => s
lbracket = serialize  '['

rbracket :: SerializeValue s Char => s
rbracket = serialize  ']'

bspace :: SerializeValue s Char => s
bspace = serialize  ' '

blt :: SerializeValue s Char => s
blt = serialize  '<'

bgt :: SerializeValue s Char => s
bgt = serialize  '>'

newline :: SerializeValue s Char => s
newline = serialize  '\n'

noPdfObject :: Monoid s => s
noPdfObject = mempty

espacePDFGlyph :: PDFGlyph -> EscapedPDFGlyph 
espacePDFGlyph (PDFGlyph t) = EscapedPDFGlyph . C.concatMap escapeByteString $ t

espaceAsciiString :: AsciiString -> EscapedAsciiString 
espaceAsciiString (AsciiString t) = EscapedAsciiString . C.concatMap escapeByteString $ t
    
instance PdfObject PDFString where
  toPDF a = mconcat [ blt
                    , fromByteString $ toHexaStream a
                    , bgt
                    ]

instance PdfLengthInfo PDFString where

instance PdfObject PDFGlyph where
  toPDF a = mconcat [ lparen
                    , serialize . espacePDFGlyph $ a 
                    , rparen
                    ]

instance PdfLengthInfo PDFGlyph where


instance PdfLengthInfo AsciiString where

instance PdfObject AsciiString where
  toPDF a = mconcat [ lparen
                    , serialize . espaceAsciiString $ a 
                    , rparen
                    ]

-- | A PDFName object
newtype PDFName = PDFName String deriving(Eq,Ord)

instance PdfObject PDFName where
 toPDF (PDFName a) = serialize ("/" ++ a)

instance PdfLengthInfo PDFName where

 
-- | A PDFArray
type PDFArray = [AnyPdfObject]

instance PdfObject a => PdfObject [a] where
    toPDF l = mconcat $ (lbracket:intersperse bspace (map toPDF l)) ++ [bspace] ++ [rbracket]
       
instance PdfObject a => PdfLengthInfo [a] where

-- | A PDFDictionary

newtype PDFDictionary = PDFDictionary (M.Map PDFName AnyPdfObject)

instance PdfObject PDFDictionary where
  toPDF (PDFDictionary a) = mconcat $ [blt,blt,newline]
                                       ++ [convertLevel a]
                                       ++ [bgt,bgt] 
   where
     convertLevel _ = let convertItem key value current = mconcat $ [ toPDF key
                                                                    , bspace
                                                                    , toPDF value
                                                                    , newline
                                                                    , current
                                                                    ]
                                                                       
          in 
           M.foldrWithKey convertItem mempty a
  
instance PdfLengthInfo PDFDictionary where

-- | Am empty dictionary
emptyDictionary :: PDFDictionary
emptyDictionary = PDFDictionary M.empty
           
isEmptyDictionary :: PDFDictionary -> Bool
isEmptyDictionary (PDFDictionary d) = M.null d

insertInPdfDict :: PDFName -> AnyPdfObject -> PDFDictionary -> PDFDictionary
insertInPdfDict key obj (PDFDictionary d) = PDFDictionary $ M.insert key obj d

pdfDictUnion :: PDFDictionary -> PDFDictionary -> PDFDictionary
pdfDictUnion (PDFDictionary a) (PDFDictionary b) = PDFDictionary $ M.union a b

  
-- | A PDF rectangle
data PDFRect = PDFRect !Int !Int !Int !Int
  
instance PdfObject PDFRect where
 toPDF (PDFRect a b c d) = toPDF . map (AnyPdfObject . PDFInteger) $ [a,b,c,d]
 
instance PdfLengthInfo PDFRect where

      
-- | A Referenced objects
data PDFReferencedObject a = PDFReferencedObject !Int !a

instance PdfObject a => PdfObject (PDFReferencedObject a) where
  toPDF (PDFReferencedObject referenceId obj) =
    mconcat  $ [ serialize . show $ referenceId
               , serialize (" 0 obj" :: String)
               , newline
               , toPDF obj
               , newline
               , serialize ("endobj" :: String)
               , newline , newline
               ]

instance PdfObject a => PdfLengthInfo (PDFReferencedObject a) where

               
-- | A reference to a PDF object
data PDFReference s = PDFReference !Int deriving(Eq,Ord,Show)

-- | Get the reference value
referenceValue :: PDFReference s -> Int
referenceValue (PDFReference i) = i

instance PdfObject s => Num (PDFReference s) where
  (+) (PDFReference a) (PDFReference b) = PDFReference (a+b)
  (*) (PDFReference a) (PDFReference b) = PDFReference (a*b)
  negate (PDFReference a) = PDFReference (negate a)
  abs (PDFReference a) = PDFReference (abs a)
  signum (PDFReference a) = PDFReference (signum a)
  fromInteger a = PDFReference (fromInteger a)

instance PdfObject s => PdfObject (PDFReference s) where
  toPDF (PDFReference i) = mconcat $ [ serialize . show $ i
                                     , serialize (" 0 R" :: String)]
                                      
                                   
instance PdfObject s => PdfLengthInfo (PDFReference s) where
               
instance (PdfObject a,PdfObject b) => PdfObject (Either a b) where
  toPDF (Left a) = toPDF a
  toPDF (Right a) = toPDF a

instance (PdfObject a, PdfObject b) => PdfLengthInfo (Either a b) where

modifyStrict :: (MonadState s m) => (s -> s) -> m ()
modifyStrict f = do
  	s <- get
  	put $! (f s)
  	
-- | A monad where paths can be created
class MonadWriter Builder m => MonadPath m