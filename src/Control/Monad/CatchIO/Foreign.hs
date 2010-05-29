{-# LANGUAGE CPP, MagicHash, UnboxedTuples, PatternSignatures #-}
module Control.Monad.CatchIO.Foreign
    ( alloca
    , allocaBytes
    , allocaArray
    , allocaArray0
    , withForeignPtr
    )
    where

#ifdef MIN_VERSION_transformers
import Control.Monad.IO.Class(liftIO)
#else
import Control.Monad.Trans (liftIO)
#endif

import Control.Monad.CatchIO

import qualified Foreign as F
import Foreign (Ptr,ForeignPtr, sizeOf, alignment, Storable)

#ifdef __GLASGOW_HASKELL__
import qualified Data.Primitive as P
import GHC.Exts
import GHC.IOBase hiding (liftIO)
#endif

alloca :: (F.Storable a, MonadCatchIO m) => (Ptr a -> m b) -> m b
alloca  = doAlloca undefined
  where
    doAlloca       :: (MonadCatchIO m', Storable a') => a' -> (Ptr a' -> m' b') -> m' b'
    doAlloca dummy  = allocaBytesAligned (F.sizeOf dummy) (F.alignment dummy)

allocaBytes :: (MonadCatchIO m) => Int -> (Ptr a -> m b) -> m b
allocaBytes size = bracket (liftIO $ F.mallocBytes size) (liftIO . F.free)

allocaBytesAligned :: (MonadCatchIO m) => Int -> Int -> (Ptr a -> m b) -> m b
#ifdef __GLASGOW_HASKELL__
allocaBytesAligned size alignment k
 = do
  (ba :: P.ByteArray) <- liftIO $ P.newAlignedPinnedByteArray size alignment >>= P.unsafeFreezeByteArray
  r <- k $ case P.byteArrayContents ba of
             P.Addr addr# -> Ptr addr#
  liftIO $ touch ba
  return r

touch :: a -> IO ()
touch a = IO $ \s -> case touch# a s of
                       s' -> (# s', () #)

#else
allocaBytesAligned size _ = allocaBytes size -- wrong
#endif

allocaArray :: (F.Storable a, MonadCatchIO m) => Int -> (Ptr a -> m b) -> m b
allocaArray  = doAlloca undefined
  where
    doAlloca            :: (Storable a', MonadCatchIO m') => a' -> Int -> (Ptr a' -> m' b') -> m' b'
    doAlloca dummy size  = allocaBytesAligned (size * sizeOf dummy) (alignment dummy)

allocaArray0 :: (F.Storable a, MonadCatchIO m) => Int -> (Ptr a -> m b) -> m b
allocaArray0 size = allocaArray (size + 1)

withForeignPtr :: (MonadCatchIO m) => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr fo io
    = do r <- io (F.unsafeForeignPtrToPtr fo)
         liftIO $ F.touchForeignPtr fo
         return r
