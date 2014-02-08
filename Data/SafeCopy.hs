{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes, FlexibleInstances, UndecidableInstances, Unsafe #-}

module Data.SafeCopy (
    Transfer(..),
    DeepTransfer(..),
    PrimTransfer,
    trPair,
    trEither,
    trList,
    trPrim,
    copy
) where

import GHC.Exts
import GHC.Base
import Control.Exception
import Control.Applicative
import Unsafe.Coerce

import Data.Int
import Data.Word

-- TODO: make this safer

-- What does a transfer function do? Two things:
--  * It deep-forces its input to rnf
--  * It copies its input

newtype Transfer a = Transfer { transfer :: a -> IO a }

trPair :: Transfer a -> Transfer b -> Transfer (a, b)
trPair t1 t2 = Transfer $ \(a, b) -> (,) <$> transfer t1 a <*> transfer t2 b

instance (DeepTransfer a, DeepTransfer b) => DeepTransfer (a, b) where
    deepTransfer = trPair deepTransfer deepTransfer

trEither :: Transfer a -> Transfer b -> Transfer (Either a b)
trEither t1 t2 = Transfer f
    where f (Left x)  = Left  <$> transfer t1 x
          f (Right x) = Right <$> transfer t2 x

instance (DeepTransfer a, DeepTransfer b) => DeepTransfer (Either a b) where
    deepTransfer = trEither deepTransfer deepTransfer

trList :: Int -> Transfer a -> Transfer [a]
trList n t = Transfer (\xs -> f xs [])
    where f [] r = return (reverse r)
          f (x:xs) r | n == 0    = return (reverse (error "trList: truncated" : r))
                     | otherwise = f xs . (: r) =<< transfer t x

class DeepTransfer a where
    deepTransfer :: Transfer a

instance PrimTransfer a => DeepTransfer a where
    deepTransfer = trPrim

class PrimTransfer a where
instance PrimTransfer Bool
instance PrimTransfer Char
instance PrimTransfer Double
instance PrimTransfer Float
instance PrimTransfer Int
instance PrimTransfer Int8
instance PrimTransfer Int16
instance PrimTransfer Int32
instance PrimTransfer Int64
instance PrimTransfer Integer
instance PrimTransfer Word
instance PrimTransfer Word8
instance PrimTransfer Word16
instance PrimTransfer Word32
instance PrimTransfer Word64
instance PrimTransfer Ordering
instance PrimTransfer ()

trPrim :: PrimTransfer a => Transfer a
trPrim = Transfer copy

copy :: a -> IO a
copy r = do
    x <- evaluate (unsafeCoerce r)
    y <- IO $ \s -> dupClosure# x s
    return (unsafeCoerce y)

foreign import prim "dupClosurezh" dupClosure# :: Any -> State# RealWorld -> (# State# RealWorld, Any #)
