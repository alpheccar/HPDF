{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- Serializer
---------------------------------------------------------
-- #hide
module Graphics.PDF.LowLevel.Serializer(
  SerializeValue(..)
 ) where
   
   
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Data.Word 
import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Builder as BU
import qualified Data.ByteString.Lazy.Char8 as C
import Foreign.Ptr(Ptr)
#if __GLASGOW_HASKELL__ >= 608
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Internal as L(ByteString(..))
#else
import Data.ByteString.Base
import qualified Data.ByteString.Base as L(LazyByteString(..))
#endif
import System.IO.Unsafe

foreign import ccall "conversion.h c_floatToString" cfloatToString :: Double -> Ptr Word8 -> IO Int
foreign import ccall "conversion.h c_shortToString" cshortToString :: Int -> Ptr Word8 -> IO Int



class (Monoid s) => SerializeValue s a where
    serialize :: a -> s
    cons :: a -> s -> s
    cons a b = (serialize a) `mappend` b
    
instance SerializeValue B.ByteString Word8 where
    serialize = B.singleton
    cons = B.cons
    
instance SerializeValue B.ByteString Char where
    serialize = C.singleton
    cons = C.cons  
    
instance SerializeValue B.ByteString [Char] where
    serialize = C.pack
    
instance SerializeValue B.ByteString B.ByteString where
    serialize = id
    
convertShort :: Int -> ByteString    
convertShort a = unsafePerformIO (createAndTrim 12 (cshortToString a))
{-# NOINLINE convertShort #-}

convertFloat :: Double -> ByteString  
convertFloat a = unsafePerformIO (createAndTrim 12 (cfloatToString a))
{-# NOINLINE convertFloat #-}
    
#if __GLASGOW_HASKELL__ >= 608
instance SerializeValue B.ByteString Int where
    serialize a = L.Chunk (convertShort a) L.Empty
 
instance SerializeValue B.ByteString Double where
    serialize a = L.Chunk (convertFloat a) L.Empty
#else
instance SerializeValue L.LazyByteString Int where
    serialize a = L.LPS [convertShort a]

instance SerializeValue L.LazyByteString Double where
    serialize a = L.LPS [convertFloat a] 
#endif
    
instance SerializeValue BU.Builder Word8 where
    serialize = BU.singleton

instance SerializeValue BU.Builder Char where
    serialize = BU.singleton . c2w
    
instance SerializeValue BU.Builder [Char] where
    serialize = BU.fromLazyByteString . serialize

instance SerializeValue BU.Builder B.ByteString where
    serialize = BU.fromLazyByteString

instance SerializeValue BU.Builder Int where
    serialize = BU.fromLazyByteString . serialize

instance SerializeValue BU.Builder Double where
    serialize = BU.fromLazyByteString . serialize

    
