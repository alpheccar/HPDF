{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- AFM Parser
---------------------------------------------------------
module Graphics.PDF.Fonts.Encoding(
      getEncoding
    , Encodings(..)
    , PostscriptName
    , parseMacEncoding
    ) where 

import Graphics.PDF.LowLevel.Types
import qualified Data.Map.Strict as M
import System.FilePath 
import Paths_HPDF
import qualified Data.ByteString.Char8 as C 
import Data.Char(digitToInt)
import Data.Maybe(mapMaybe)

type PostscriptName = String 

data Encodings = AdobeStandardEncoding 
               | ZapfDingbatsEncoding
               deriving(Eq)

isLine :: C.ByteString -> Bool 
isLine c | not (C.null c) = C.head c /= '#'
         | otherwise = False

from4Hexa :: C.ByteString -> Int 
from4Hexa a = sum . map (\(x,y) -> x * y) $ zip (map digitToInt . C.unpack $ a)  (map (\x -> 16^x) ([3,2,1,0] :: [Integer]))

from3Octal:: C.ByteString -> Int 
from3Octal a = sum . map (\(x,y) -> x * y) $ zip (map digitToInt . C.unpack $ a)  (map (\x -> 8^x) ([2,1,0] :: [Integer]))


toData :: [C.ByteString] -> Maybe (PostscriptName,Char)
toData (a:b:_) = Just (C.unpack a,toEnum . from4Hexa $ b)
toData _ = Nothing

toMacData :: [C.ByteString] -> Maybe (PostscriptName,GlyphCode)
toMacData (name:_:mac:_) | C.unpack mac == "-" = Nothing
                         | otherwise = Just (C.unpack name,fromIntegral (from3Octal mac))
toMacData _ = Nothing

parseGlyphListEncoding :: String -> IO (M.Map PostscriptName Char)
parseGlyphListEncoding name = do 
	path <- getDataFileName name 
	l <- C.readFile path 
	return (M.fromList . mapMaybe (toData  . C.split ';') . filter isLine . C.lines $ l)

parseMacEncoding :: IO (M.Map PostscriptName GlyphCode)
parseMacEncoding = do 
    path <- getDataFileName "Encodings/pdfencodings.txt" 
    l <- C.readFile path 
    return . M.fromList . mapMaybe (toMacData . C.split '\t') . tail . C.lines $ l


getEncoding :: Encodings -> IO (M.Map PostscriptName Char)
getEncoding AdobeStandardEncoding = parseGlyphListEncoding $ "Encodings" </> "glyphlist" <.> "txt"
getEncoding ZapfDingbatsEncoding= parseGlyphListEncoding $ "Encodings" </> "zapfdingbats" <.> "txt"
