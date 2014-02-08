module Control.Monad.RLimits (

    -- Monad
    CM,
    startCM,
    getCurrentLabel,

    -- Data types
    RC,
    RCSet,
    RCRef,
    RCMVar,
    RCResult,

    -- RCRef
    newRCRef,
    readRCRef,
    copyRCRef,
    writeRCRef,
    newRCMVar,

    -- RCMVar
    newEmptyRCMVar,
    takeRCMVar,
    copyRCMVar,
    putRCMVar,

    -- Actions
    withRC,
    newRC,
    rcKill,
    rcFork,
    rcCopyResult,

    -- Convenience
    su, ss,

    -- Unsafe
    liftIOTCB

) where

import qualified Data.IntSet as S
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IORef
import Data.Maybe
import System.Mem.Weak
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Concurrent.MVar
import System.IO.Unsafe
import qualified Data.IntMap.Strict as M

import qualified Control.RLimits as R
import Data.SafeCopy

-- Modeled off of LIO proper, but with many simplifications.

-- XXX TODO logic for transfers is not quite right; they should
-- get killed too but softly

-- | A chain of RCRefs, so that if any single container is dead, then
-- the entire reference chain is dead.
data AllRCRef a = RCTerminal a
                | RCDead
                | RCGate (R.RCRef (AllRCRef a))

data StateTCB = StateTCB {
    label :: IORef RCSet, -- synchronized by 'killed'
    tid :: Int,
    global :: MVar GlobalStateTCB
}

data GlobalStateTCB = GlobalStateTCB {
    killed :: IntSet,
    tidpool :: Int,
    threads :: IntMap (Weak ThreadId, IORef RCSet)
}

startCM :: CM () -> IO ()
startCM m = do
    evaluate globalRCMap
    g <- newMVar (GlobalStateTCB S.empty 0 M.empty)
    label <- newIORef (S.singleton 0)
    let st = StateTCB label (-1) g
    runCM (rcFork (S.singleton 0) m >>= \x -> rcCopyResult x trPrim >> return ()) st

newtype CM a = CM { runCM :: StateTCB -> IO a }
instance Functor CM where
    fmap f (CM m) = CM (fmap (fmap f) m)
instance Applicative CM where
    pure = return
    f <*> x = do
        f' <- f
        fmap f' x
instance Monad CM where
    return x = CM (\_ -> return x)
    (CM m) >>= f = CM (\s -> do
        x <- m s
        runCM (f x) s)

liftIOTCB :: IO a -> CM a
liftIOTCB m = CM (\_ -> m)

withGlobalStateTCB :: (GlobalStateTCB -> IO a) -> CM a
withGlobalStateTCB f = CM (\s -> withMVar (global s) f)

modifyGlobalStateTCB_ :: (GlobalStateTCB -> IO GlobalStateTCB) -> CM ()
modifyGlobalStateTCB_ f = CM (\s -> modifyMVar_ (global s) f)

getStateTCB :: CM StateTCB
getStateTCB = CM (\s -> return s)

getCurrentLabel :: CM RCSet
getCurrentLabel = CM (\s -> readIORef (label s))

intersects x y = not (S.null (x `S.intersection` y))

type RC = Int
type RCSet = IntSet
data LObj o = LObjTCB !RCSet !o
type RCRef a = LObj (IORef (AllRCRef a)) -- strict!
type RCMVar a = LObj (MVar (AllRCRef a))
type RCResult a = LObj (MVar (Maybe (AllRCRef a)))

canFlowTo :: RCSet -> RCSet -> Bool
canFlowTo s1 s2 = s1 `S.isSubsetOf` s2

transmuteRC :: RC -> IO R.RC
transmuteRC p = withMVar globalRCMap (\(_, m) -> return (fromMaybe (error "transmuteRC: Uninitialized principal") (M.lookup p m)))

{-# NOINLINE globalRCMap #-}
globalRCMap :: MVar (Int, IntMap R.RC)
globalRCMap = unsafePerformIO $ do
    rc <- R.getCurrentRC
    newMVar (1, M.singleton 0 rc)

readAllRCRef :: AllRCRef a -> IO (Maybe a)
readAllRCRef (RCTerminal a) = return (Just a)
readAllRCRef RCDead = return Nothing
readAllRCRef (RCGate r) = do
    maybe (return Nothing) readAllRCRef =<< R.readRCRef r

wrapRCRef :: RCSet -> a -> IO (AllRCRef a)
wrapRCRef d a = wrapM (S.toAscList d) (RCTerminal a)
    where wrapM [] r = return r
          wrapM (p:ps) r = do
            rc <- transmuteRC p
            ref <- R.newRCRef rc r
            wrapM ps (RCGate ref)

newRCRef :: a -> RCSet -> CM (RCRef a)
newRCRef x l = do
    -- check?
    r <- liftIOTCB $ newIORef =<< wrapRCRef l x
    return (LObjTCB l r)

readRCRef :: RCRef a -> CM a
readRCRef (LObjTCB l r) = do
    c <- getCurrentLabel
    st <- getStateTCB
    withGlobalStateTCB $ \g -> do
        when (intersects l (killed g)) $ error "readRCRef: no longer available"
        writeIORef (label st) (c `S.union` l)
    m <- liftIOTCB $ readAllRCRef =<< readIORef r
    case m of
        Nothing -> error "readRCRef: no longer available"
        Just x -> return x

copyRCRef :: RCRef a -> Transfer a -> CM a
copyRCRef (LObjTCB _ r) t = do
    m <- liftIOTCB $ readAllRCRef =<< readIORef r
    case m of
        Nothing -> error "copyRCRef: no longer available"
        Just x -> liftIOTCB $ transfer t x

writeRCRef :: RCRef a -> a -> CM ()
writeRCRef (LObjTCB l r) t = do
    t <- liftIOTCB $ evaluate t
    c <- getCurrentLabel
    unless (c `canFlowTo` l) $ error "writeRCRef: bad write"
    liftIOTCB $ writeIORef r =<< wrapRCRef l t

newRCMVar :: a -> RCSet -> CM (RCMVar a)
newRCMVar x l = do
    -- check?
    r <- liftIOTCB $ newMVar =<< wrapRCRef l x
    return (LObjTCB l r)

newEmptyRCMVar :: RCSet -> CM (RCMVar a)
newEmptyRCMVar l = do
    -- check?
    r <- liftIOTCB $ newEmptyMVar
    return (LObjTCB l r)

takeRCMVar :: RCMVar a -> CM a
takeRCMVar (LObjTCB l r) = do
    c <- getCurrentLabel
    st <- getStateTCB
    withGlobalStateTCB $ \g -> do
        when (intersects l (killed g)) $ error "takeRCMVar: no longer available"
        writeIORef (label st) (c `S.union` l)
    m <- liftIOTCB $ readAllRCRef =<< takeMVar r
    case m of
        Nothing -> error "takeRCMVar: no longer available (hm)"
        Just x -> return x

copyRCMVar :: RCMVar a -> Transfer a -> CM a
copyRCMVar (LObjTCB _ r) t = do
    m <- liftIOTCB $ readAllRCRef =<< takeMVar r
    case m of
        Nothing -> error "copyRCMVar: no longer available"
        Just x -> liftIOTCB $ transfer t x

putRCMVar :: RCMVar a -> a -> CM ()
putRCMVar (LObjTCB l r) t = do
    c <- getCurrentLabel
    unless (c `canFlowTo` l) . error $ "putRCMVar: bad write " ++ show c ++ " to " ++ show l
    liftIOTCB $ putMVar r =<< wrapRCRef l t

withRC :: RC -> CM a -> CM a
withRC p m = do
    c <- getCurrentLabel
    st <- getStateTCB
    withGlobalStateTCB $ \g -> do
        when (S.member p (killed g)) $ error "withRC: cannot switch into dead container"
        writeIORef (label st) (S.insert p c)
    rc <- liftIOTCB $ transmuteRC p
    s <- getStateTCB
    liftIOTCB $ R.withRC rc (runCM m s)

newRC :: Int -> CM RC
newRC limit = liftIOTCB $ do
    parent_rc <- R.getCurrentRC
    modifyMVar globalRCMap $ \(i,m) -> do
        let i' = i + 1
        rc <- R.newRC (fromIntegral limit) parent_rc
        return ((i', M.insert i' rc m), i')

rcKill :: RC -> CM ()
rcKill p = do
    modifyGlobalStateTCB_ $ \g -> do
        if p `S.member` killed g
            then return g
            else do
                let killed' = S.insert p (killed g)
                R.killRC =<< transmuteRC p
                threads' <- (\f -> foldM f (threads g) (M.toList (threads g))) $ \m (i, (wtid, sr)) -> do
                    s <- readIORef sr
                    if (p `S.member` s)
                        then do
                            mtid <- deRefWeak wtid
                            case mtid of
                                Nothing -> return ()
                                Just tid -> killThread tid
                            return (M.delete i m)
                        else return m
                return (g {killed = killed', threads = threads'})

rcFork :: RCSet -> CM a -> CM (RCResult a)
rcFork l m = do
    c <- getCurrentLabel
    st <- getStateTCB
    label' <- liftIOTCB $ newIORef c
    result <- liftIOTCB $ newEmptyMVar
    let handler m = m `catch` h
            where h HeapOverflow = putMVar result Nothing
                  h _ = return ()
    liftIOTCB . forkIO . handler . flip runCM (st { label = label' }) $ do
        tid <- liftIOTCB $ myThreadId
        modifyGlobalStateTCB_ $ \g -> do
            let i = tidpool g
            when (c `intersects` killed g) $ error "rcFork: parent thread died!"
            let cleanup = modifyMVar_ (global st) $ \g -> do
                    return (g {threads = M.delete i (threads g)})
            wtid <- mkWeak tid tid (Just cleanup)
            return (g {tidpool = i + 1, threads = M.insert i (wtid, label') (threads g)})
        x <- m
        c <- getCurrentLabel
        if (c `canFlowTo` l)
            then liftIOTCB $ putMVar result . Just =<< wrapRCRef l x
            else liftIOTCB $ putMVar result Nothing
    return (LObjTCB l result)

rcCopyResult :: RCResult a -> Transfer a -> CM a
rcCopyResult (LObjTCB _ mvar) t = liftIOTCB $ do
    rcref <- readMVar mvar
    case rcref of
        Nothing -> return (error "rcCopyResult: no longer available (1)")
        Just rcref -> do
            m <- readAllRCRef rcref
            case m of
                Nothing -> return (error "rcCopyResult: no longer available (2)")
                Just x -> transfer t x

ss = S.singleton
su = S.union
