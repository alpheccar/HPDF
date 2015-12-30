{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Images
---------------------------------------------------------

module Graphics.PDF.Image(
   -- * Images
   -- ** Types
     PDFJpeg
   , JpegFile
   , RawImage
   , PDFFilter(..)
   -- ** Functions
   , createPDFJpeg
   , readJpegFile
   , jpegBounds
   , readJpegDataURL
   , createPDFRawImageFromARGB
   , createPDFRawImageFromByteString
 ) where
     
import Graphics.PDF.LowLevel.Types
import qualified Data.Map as M
import Graphics.PDF.Draw
import Graphics.PDF.Resources
import Graphics.PDF.Pages
import qualified Data.ByteString.Lazy as B
import Control.Monad.Writer
#if __GLASGOW_HASKELL__ >= 608
import System.IO hiding(withFile)
#else
import System.IO
#endif
import Data.Char(ord)
import Data.Bits
#if __GLASGOW_HASKELL__ >= 710
import qualified Control.Monad.Except as EXC
#else
import qualified Control.Monad.Error as EXC
#endif
import Graphics.PDF.Coordinates
import Data.Binary.Builder(Builder,fromLazyByteString,fromByteString)
import Control.Exception as E
import qualified Data.Vector.Unboxed as U
import Data.Word
import qualified Data.ByteString.Char8 as C8 (ByteString, pack, index, length)
import Data.ByteString.Base64(decode)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Error.Util (note)

-- | A Jpeg file   
data JpegFile = JpegFile !Int !Int !Int !Int !Builder

data PDFFilter = ASCIIHexDecode
               | ASCII85Decode
               | LZWDecode
               | FlateDecode
               | RunLengthDecode
               | CCITTFaxDecode
               | DCTDecode 
               | NoFilter

m_sof0 :: Int
m_sof0 = 0xc0 
m_sof1 :: Int 
m_sof1 = 0xc1 
--m_sof2 :: Int 
--m_sof2 = 0xc2  
m_sof3 :: Int 
m_sof3 = 0xc3  
m_sof5 :: Int 
m_sof5 = 0xc5 
m_sof6 :: Int  
m_sof6 = 0xc6 
m_sof7 :: Int  
m_sof7 = 0xc7  
--m_jpg :: Int 
--m_jpg = 0xc8  
m_sof9 :: Int  
m_sof9 = 0xc9  
m_sof10 :: Int 
m_sof10 = 0xca
m_sof11 :: Int 
m_sof11 = 0xcb
m_sof13 :: Int 
m_sof13 = 0xcd 
m_sof14 :: Int 
m_sof14 = 0xce 
m_sof15 :: Int 
m_sof15 = 0xcf 
--m_dht :: Int 
--m_dht = 0xc4   
--m_dac :: Int 
--m_dac = 0xcc   
m_rst0 :: Int              
m_rst0 = 0xd0  
m_rst1 :: Int 
m_rst1 = 0xd1 
m_rst2 :: Int  
m_rst2 = 0xd2  
m_rst3 :: Int 
m_rst3 = 0xd3
m_rst4 :: Int   
m_rst4 = 0xd4 
m_rst5 :: Int  
m_rst5 = 0xd5
m_rst6 :: Int   
m_rst6 = 0xd6 
m_rst7 :: Int  
m_rst7 = 0xd7  
m_soi :: Int 
m_soi = 0xd8 
m_eoi :: Int   
m_eoi = 0xd9 
--m_sos :: Int   
--m_sos = 0xda
--m_dqt :: Int    
--m_dqt = 0xdb 
--m_dnl :: Int   
--m_dnl = 0xdc
--m_dri :: Int    
--m_dri = 0xdd
--m_dhp :: Int    
--m_dhp = 0xde
--m_exp :: Int    
--m_exp = 0xdf
--m_app0 :: Int    
--m_app0 = 0xe0  
--m_app1 :: Int 
--m_app1 = 0xe1  
--m_app2 :: Int 
--m_app2 = 0xe2  
--m_app3 :: Int 
--m_app3 = 0xe3  
--m_app4 :: Int 
--m_app4 = 0xe4 
--m_app5 :: Int  
--m_app5 = 0xe5  
--m_app6 :: Int 
--m_app6 = 0xe6  
--m_app7 :: Int 
--m_app7 = 0xe7 
--m_app8 :: Int  
--m_app8 = 0xe8  
--m_app9 :: Int 
--m_app9 = 0xe9  
--m_app10 :: Int 
--m_app10 = 0xea
--m_app11 :: Int 
--m_app11 = 0xeb
--m_app12 :: Int 
--m_app12 = 0xec
--m_app13 :: Int 
--m_app13 = 0xed
--m_app14 :: Int 
--m_app14 = 0xee
--m_app15 :: Int 
--m_app15 = 0xef
--m_jpg0 :: Int 
--m_jpg0 = 0xf0 
--m_jpg13 :: Int 
--m_jpg13 = 0xfd
--m_com :: Int 
--m_com = 0xfe
m_tem :: Int 
m_tem = 0x01 
--m_error :: Int 
--m_error = 0x100 
   
io :: IO a -> FA a 
io = FA . liftIO

-- | File analyzer monad
#if __GLASGOW_HASKELL__ >= 710
newtype FA a = FA { unFA :: EXC.ExceptT String IO a}
#else
newtype FA a = FA { unFA :: EXC.ErrorT String IO a}
#endif
#ifndef __HADDOCK__
  deriving(Monad,Applicative,EXC.MonadError String,Functor)
#else
instance Monad FA
instance MonadError String FA
instance MonadIO FA
instance Functor FA
#endif
    
runFA :: FA a -> IO (Either String a)
#if __GLASGOW_HASKELL__ >= 710
runFA = EXC.runExceptT . unFA
#else
runFA = EXC.runErrorT . unFA
#endif

readWord16 :: Handle -> FA Int
readWord16 h = io $ do
    hi <- hGetChar h
    lo <- hGetChar h
    return $ ((fromEnum hi) `shiftL` 8) .|. (fromEnum . ord $ lo)

readWord8 :: Handle -> FA Int
readWord8 h = io $ do
    lo <- hGetChar h
    return $ fromEnum . ord $ lo
            
--optional :: FA (Maybe a) -> FA (Maybe a)
--optional a = a --`catchError` (\e -> return Nothing)

--jfif :: Handle -> FA (Maybe (Double,Double))
--jfif h = do
--     header <- readWord16 h
--     when (header /= 0x0FFE0) $ throwError (strMsg "No JFIF magic number")
--     readWord16 h
--     mapM_ check "JFIF"
--     readWord16 h
--     unit <- readWord8 h
--     width <- fromIntegral `fmap` readWord16 h
--     height <- fromIntegral `fmap` readWord16 h
--     case unit of
--         1 -> return $ Just (width,height)
--         2 -> return $ Just (width*2.54,height*2.54)
--         _ -> return $ Just (0,0)
--    where
--     check c' = do
--         c <- io $ hGetChar h
--         when (c /= c') $ throwError (strMsg "No JFIF header")
           
parseJpegContent :: Handle -> FA (Int,Int,Int,Int)
parseJpegContent h = do
    r <- readWord8 h
    when (r /=  0x0FF) $ EXC.throwError "No marker found"
    sof <- readWord8 h
    case sof of
        a | a `elem` [m_sof5,m_sof6,m_sof7,m_sof9,m_sof10,m_sof11,m_sof13,m_sof14,m_sof15] ->
                EXC.throwError "Unuspported compression mode"
          | a `elem` [m_sof0,m_sof1,m_sof3] -> do
              _ <- readWord16 h
              bits_per_component <- readWord8 h
              height <- readWord16 h
              width <- readWord16 h
              color_space  <- readWord8 h
              return (bits_per_component,height,width,color_space)                  
          | a `elem` [m_soi,m_eoi,m_tem,m_rst0,m_rst1,m_rst2,m_rst3,m_rst4,m_rst5,m_rst6,m_rst7] -> parseJpegContent h
          | otherwise -> do
               l <- readWord16 h
               io $ hSeek h RelativeSeek (fromIntegral (l-2))
               parseJpegContent h

analyzeJpeg :: Handle -> FA (Int,Int,Int,Int)
analyzeJpeg h = do
    -- Get Length
    io $ hSeek h SeekFromEnd 0
    --fileLength <- io $ hTell h
    io $ hSeek h AbsoluteSeek 0
    -- Check jpeg
    header <- readWord16 h
    when (header /= 0x0FFD8) $ EXC.throwError "Not a JPEG File"
   
    -- Extract resolution from jfif
    --res <- optional $ jfif h
    
    io $ hSeek h AbsoluteSeek 0
    
    (bits_per_component,height,width,color_space) <- parseJpegContent h
    --io $ print fileLength
    --io $ print res
    --io $ print bits_per_component
    --io $ print height
    --io $ print width
    --io $ print color_space
    --io $ hClose h
    unless (color_space `elem` [1,3,4]) $ EXC.throwError "Color space not supported"
    return (bits_per_component,height,width,color_space)
    
--test = analyzePng "Test/logo.png"
    
withFile :: String -> (Handle -> IO c) -> IO c    
withFile name = bracket (openBinaryFile name ReadMode) hClose

-- | Read a JPEG file and return an abstract description of its content or an error
-- The read is not lazy. The whole image will be loaded into memory
readJpegFile :: FilePath
             -> IO (Either String JpegFile)
readJpegFile f = (do
     r <- liftIO $ withFile f $ \h -> do
             runFA (analyzeJpeg h)
     case r of
         Right (bits_per_component,height,width,color_space) -> do
                 img <- liftIO $ withFile f $ \h' -> do
                     nb <- hFileSize h'
                     B.hGet h' (fromIntegral nb)
                 return (Right $ JpegFile bits_per_component width height color_space (fromLazyByteString img))
         Left s -> return $ Left s) `E.catch` (\(err :: IOException) -> return $ Left (show err)) 

-- | Get the JPEG bounds
jpegBounds :: JpegFile -> (Int,Int)
jpegBounds (JpegFile _ w h _ _) = (w,h)

-- | Use an abstract description of a Jpeg to return a PDFReference that can be used to manipulate the Jpeg in the context
-- of the PDF document
createPDFJpeg :: JpegFile  
              -> PDF (PDFReference PDFJpeg)
createPDFJpeg (JpegFile bits_per_component width height color_space img) = do
        PDFReference s <- createContent a' Nothing  
        recordBound s (fromIntegral width) (fromIntegral height)
        return (PDFReference s) 
    where
       color c = case c of
           1 -> [(PDFName "ColorSpace",AnyPdfObject $ PDFName "DeviceGray")]
           3 -> [(PDFName "ColorSpace",AnyPdfObject $ PDFName "DeviceRGB")]
           4 -> [(PDFName "ColorSpace",AnyPdfObject $ PDFName "DeviceCMYK")
                ,(PDFName "Decode",AnyPdfObject . map (AnyPdfObject . PDFInteger) $ [1,0,1,0,1,0,1,0])
                ]
           _ -> error "Jpeg color space not supported"
       a' = 
                 do modifyStrict $ \s -> s  {otherRsrcs = PDFDictionary. M.fromList $ 
                                                   [ (PDFName "Type",AnyPdfObject . PDFName $ "XObject")
                                                   , (PDFName "Subtype",AnyPdfObject . PDFName $ "Image")
                                                   , (PDFName "Width",AnyPdfObject . PDFInteger $ width)
                                                   , (PDFName "Height",AnyPdfObject . PDFInteger $ height)
                                                   , (PDFName "BitsPerComponent",AnyPdfObject . PDFInteger $ bits_per_component)
                                                   , (PDFName "Interpolate", AnyPdfObject True)
                                                   , (PDFName "Filter",AnyPdfObject . PDFName $ "DCTDecode")
                                                   ] ++ color color_space
                                             }
                    tell img        
        
createPDFRawImageFromByteString :: Int -- ^ Width
                                -> Int -- ^ Height
                                -> Bool -- ^ Interpolation
                                -> PDFFilter -- ^ Decompression filter to be sued by the PDF reader to render the picture
                                -> B.ByteString -- ^ RGB pixels
                                -> PDF (PDFReference RawImage)  
createPDFRawImageFromByteString width height interpolate pdfFilter stream =  do
        PDFReference s <- createContent a' Nothing  
        recordBound s (fromIntegral width) (fromIntegral height)
        return (PDFReference s) 
    where     
        getFilter = case pdfFilter of 
                    NoFilter -> []
                    ASCIIHexDecode -> [(PDFName "Filter",AnyPdfObject . PDFName $ "ASCIIHexDecode")]
                    ASCII85Decode -> [(PDFName "Filter",AnyPdfObject . PDFName $ "ASCII85Decode")]
                    LZWDecode -> [(PDFName "Filter",AnyPdfObject . PDFName $ "LZWDecode")]
                    FlateDecode -> [(PDFName "Filter",AnyPdfObject . PDFName $ "FlateDecode")]
                    RunLengthDecode -> [(PDFName "Filter",AnyPdfObject . PDFName $ "RunLengthDecode")]
                    CCITTFaxDecode -> [(PDFName "Filter",AnyPdfObject . PDFName $ "CCITTFaxDecode")]
                    DCTDecode -> [(PDFName "Filter",AnyPdfObject . PDFName $ "DCTDecode")]

        a' =  do 
                modifyStrict $ \s -> s  {otherRsrcs = PDFDictionary. M.fromList $ 
                                                   [ (PDFName "Type",AnyPdfObject . PDFName $ "XObject")
                                                   , (PDFName "Subtype",AnyPdfObject . PDFName $ "Image")
                                                   , (PDFName "Width",AnyPdfObject . PDFInteger $ width)
                                                   , (PDFName "Height",AnyPdfObject . PDFInteger $ height)
                                                   , (PDFName "BitsPerComponent",AnyPdfObject . PDFInteger $ 8)
                                                   , (PDFName "ColorSpace",AnyPdfObject $ PDFName "DeviceRGB")
                                                   , (PDFName "Interpolate", AnyPdfObject interpolate)
                                                   ] ++ getFilter
                                             }
                tell . fromLazyByteString $ stream

createPDFRawImageFromARGB :: Int -- ^ Width
                          -> Int -- ^ Height
                          -> Bool -- ^ Interpolation
                          -> U.Vector Word32 -- ^ ARGB pixels (A component not used y the PDF document)
                          -> PDF (PDFReference RawImage)  
createPDFRawImageFromARGB width height interpolate stream =  do
        PDFReference s <- createContent a' Nothing  
        recordBound s (fromIntegral width) (fromIntegral height)
        return (PDFReference s) 
    where
        addPixel (a:t) =  
           let xa = fromIntegral $ (a `shiftR` 16) .&. 0x0FF
               xb = fromIntegral $ (a `shiftR` 8) .&. 0x0FF
               xc = fromIntegral $ (a `shiftR` 0) .&. 0x0FF
           in
           xa:xb:xc:addPixel t
        addPixel [] = []
                        
        a' =  do 
                modifyStrict $ \s -> s  {otherRsrcs = PDFDictionary. M.fromList $ 
                                                   [ (PDFName "Type",AnyPdfObject . PDFName $ "XObject")
                                                   , (PDFName "Subtype",AnyPdfObject . PDFName $ "Image")
                                                   , (PDFName "Width",AnyPdfObject . PDFInteger $  width)
                                                   , (PDFName "Height",AnyPdfObject . PDFInteger $  height)
                                                   , (PDFName "BitsPerComponent",AnyPdfObject . PDFInteger $ 8)
                                                   , (PDFName "ColorSpace",AnyPdfObject $ PDFName "DeviceRGB")
                                                   , (PDFName "Interpolate", AnyPdfObject interpolate)
                                                   ]
                                             }
                tell . fromLazyByteString . B.pack . addPixel . U.toList $ stream  

-- Read Jpeg from ByteString
sIndex :: C8.ByteString -> Int -> Maybe Char
sIndex bs idx = 
  if (idx < 0) || (idx > C8.length bs)
  then Nothing
  else Just $ bs `C8.index` idx

sReadWord8 :: C8.ByteString -> Int -> Maybe Int
sReadWord8 bs idx = (fromEnum . ord) <$> (bs `sIndex` idx)

sReadWord16 :: C8.ByteString -> Int -> Maybe Int
sReadWord16 bs idx = do
  hi <- sReadWord8 bs idx
  lo <- sReadWord8 bs (idx + 1)
  return $ (hi `shiftL` 8) .|. lo

parseJpegDetailData :: C8.ByteString -> Int -> Maybe (Int,Int,Int,Int)
parseJpegDetailData bs offset = do
  bpc <- sReadWord8  bs (offset + 4)
  hgt <- sReadWord16 bs (offset + 5)
  wdt <- sReadWord16 bs (offset + 7)
  cls <- sReadWord8  bs (offset + 9)
  return (bpc, hgt, wdt, cls)

(?|) :: Maybe b -> a -> Either a b
(?|) = flip note

parseJpegContentData :: C8.ByteString -> Int -> Either String (Int,Int,Int,Int)
parseJpegContentData bs offset = do
  r <- sReadWord8 bs offset ?| "Corrupt JPEG data URL - no marker found"
  guard (r == 0x0FF) ?| "Corrupt JPEG data URL - no marker found"
  sof <- (sReadWord8 bs (offset + 1)) ?| "Corrupt JPEG data URL - no start of file offset found"
  case sof of
    a | a `elem` [m_sof5,m_sof6,m_sof7,m_sof9,m_sof10,m_sof11,m_sof13,m_sof14,m_sof15] -> Left "Unsupported compression mode"
      | a `elem` [m_sof0,m_sof1,m_sof3] -> (parseJpegDetailData bs offset) ?| "Corrupt JPEG data URL - insufficient data in URL"
      | a `elem` [m_soi,m_eoi,m_tem,m_rst0,m_rst1,m_rst2,m_rst3,m_rst4,m_rst5,m_rst6,m_rst7] -> parseJpegContentData bs (offset + 2)
      | otherwise -> do 
          l <- (sReadWord16 bs (offset + 2)) ?| "Corrupt JPEG data URL - insufficient data in URL"
          parseJpegContentData bs (offset + l + 2)

checkColorSpace :: (Int,Int,Int,Int) -> Either String (Int,Int,Int,Int)
checkColorSpace hdrData@(_,_,_,color_space) = do
  guard (color_space `elem` [1,3,4]) ?| ("Color space [" ++ show color_space ++ "] not supported")
  return hdrData

analyzeJpegData :: C8.ByteString -> Either String (Int,Int,Int,Int)
analyzeJpegData bs = do
  header <- sReadWord16 bs 0 ?| "Not a JPEG data URL - no marker found"
  guard (header == 0x0FFD8) ?| "Not a JPEG data URL - invalid JPEG marker" 
  hdrData <- parseJpegContentData bs 0
  checkColorSpace hdrData

readJpegData :: String -> Either String JpegFile
readJpegData dataString = do
  bs <- decode $ C8.pack dataString
  (bits_per_component,height,width,color_space) <- analyzeJpegData bs
  return $ JpegFile bits_per_component width height color_space (fromByteString bs) 

-- | Reads a data URL string, and returns a JpegFile.
-- The incoming string must be a correctly formatted data URL for a JPEG.
-- You can convert jpeg files to data URLs at the following web site:
-- http://dataurl.net/#dataurlmaker
readJpegDataURL :: String -> Either String JpegFile
readJpegDataURL dataurl = do
  guard (take 23 dataurl == "data:image/jpeg;base64,") ?| "Data URL does not start with a valid JPEG header"
  readJpegData $ drop 23 dataurl   


     
-- | A Jpeg PDF object
data PDFJpeg
instance PDFXObject PDFJpeg where
    drawXObject a = withNewContext $ do
            (width,height) <- bounds a
            applyMatrix (scale width height)
            privateDrawXObject a
        
instance PdfObject PDFJpeg where
  toPDF _ = noPdfObject

instance PdfLengthInfo PDFJpeg where

instance PdfResourceObject (PDFReference PDFJpeg) where
  toRsrc = AnyPdfObject

-- | A raw image
data RawImage

instance PDFXObject RawImage where
    drawXObject a = withNewContext $ do
            (width,height) <- bounds a
            applyMatrix (scale width height)
            privateDrawXObject a
        
instance PdfObject RawImage where
  toPDF _ = noPdfObject

instance PdfLengthInfo RawImage where

instance PdfResourceObject (PDFReference RawImage) where
  toRsrc = AnyPdfObject



