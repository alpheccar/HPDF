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
    ) where 

import Graphics.PDF.LowLevel.Types
import Data.Char 
import qualified Data.Map.Strict as M
import Graphics.PDF.Fonts.Font
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

to4Hexa :: C.ByteString -> Int 
to4Hexa a = sum . map (\(x,y) -> x * y) $ zip (map digitToInt . C.unpack $ a)  (map (\x -> 16^x) [3,2,1,0])


toData :: [C.ByteString] -> Maybe (PostscriptName,Char)
toData (a:b:_) = Just (C.unpack a,toEnum . to4Hexa $ b)
toData _ = Nothing


parseEncoding :: String -> IO (M.Map PostscriptName Char)
parseEncoding name = do 
	path <- getDataFileName name 
	l <- C.readFile path 
	return (M.fromList . mapMaybe (toData  . C.split ';') . filter isLine . C.lines $ l)


getEncoding :: Encodings -> IO (M.Map PostscriptName Char)
getEncoding AdobeStandardEncoding = parseEncoding $ "Encodings" </> "glyphlist" <.> "txt"
getEncoding ZapfDingbatsEncoding= parseEncoding $ "Encodings" </> "zapfdingbats" <.> "txt"
