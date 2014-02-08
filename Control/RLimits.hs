{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Control.RLimits (
    RC,
    Listener,
    RCRef,
    getCurrentRC,
    newRC,
    withRC,
    listenRC,
    unlistenRC,
    readRCRef,
    newRCRef
) where

import GHC.Base
import GHC.Prim

data RC = RC# RC#
data Listener = Listener# Listener#
data RCRef a = RCRef# (RCRef# a)

getCurrentRC :: a -> IO RC
getCurrentRC x = IO $ \s -> case getCurrentRC# x s of (# s', rc' #) -> (# s', RC# rc' #)

newRC :: Word -> RC -> IO RC
newRC (W# w) (RC# rc) = IO $ \s -> case newRC# w rc s of (# s', rc' #) -> (# s', RC# rc' #)

withRC :: RC -> IO a -> IO a
withRC (RC# rc) (IO m) = IO $ \s -> withRC# rc m s

-- XXX should be Word
listenRC :: RC -> Int -> IO () -> IO Listener
listenRC (RC# rc) (I# w) cb = IO $ \s -> case listenRC# rc w cb s of (# s', l #) -> (# s', Listener# l #)

unlistenRC :: Listener -> IO ()
unlistenRC (Listener# l) = IO $ \s -> unlistenRC# l s

readRCRef :: RCRef a -> IO (Maybe a)
readRCRef (RCRef# ref) = IO $ \s -> case readRCRef# ref s of
    (# s' , r , p #) -> case r of
        0# -> (# s', Nothing #)
        1# -> (# s', Just p #)

newRCRef :: RC -> a -> IO (RCRef a)
newRCRef (RC# rc) p = IO $ \s -> case newRCRef# rc p s of (# s' , r #) -> (# s' , RCRef# r #)

killRC :: RC -> IO ()
killRC (RC# rc) = IO $ \s -> case killRC# rc s of s' -> (# s' , () #)
