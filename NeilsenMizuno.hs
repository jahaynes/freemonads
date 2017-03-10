{-# LANGUAGE DeriveFunctor #-}

{- A DSL for the Neilsen-Mizuno token-passing
   distributed mutual exclusion algorithm,
   implemented using the Free Monad pattern -}

module NeilsenMizuno
        (NeilsenMizuno,
         FreeNeilsenMizuno (..),
         neilsenMizunoMain,
         neilsenMizunoReceive) where

import Control.Monad.Free   (Free (..), liftF)
import Control.Monad        (when, forever)

{- This datatype is all the actions
   we are allowed to do in this DSL 
   
   In all cases, the last parameter is
   "what to do next". -}

data (FreeNeilsenMizuno n) a

        = GetMyId (n -> a)
            
        | GetParent (Maybe n -> a)
        | SetParent (Maybe n) a

        | GetDeferred (Maybe n -> a)
        | SetDeferred (Maybe n) a

        | IsHolding (Bool -> a)
        | SetHolding Bool a

        | SendRequest n n n a
        | ReceiveRequest ((n,n) -> a)

        | SendToken n a
        | ReceiveToken a
        
        | CriticalSection a
        | NonCriticalSection a
        
        | Lock a
        | Unlock a
            deriving Functor

--Make the types look a bit nicer
type NeilsenMizuno n a = Free (FreeNeilsenMizuno n) a

-----------------------------------------------------------------------------------

-- The following functions make it easy to construct the ADT above

getMyId :: (NeilsenMizuno n) n
getMyId = liftF (GetMyId id)

getParent :: NeilsenMizuno n (Maybe n)
getParent = liftF (GetParent id)

setParent :: Maybe n -> NeilsenMizuno n ()
setParent node = liftF (SetParent node ())

getDeferred :: NeilsenMizuno n (Maybe n)
getDeferred = liftF (GetDeferred id)

setDeferred :: Maybe n -> NeilsenMizuno n ()
setDeferred node = liftF (SetDeferred node ()) 

isHolding :: NeilsenMizuno n Bool
isHolding = liftF (IsHolding id)

setHolding :: Bool -> NeilsenMizuno n ()
setHolding holding = liftF (SetHolding holding ())

sendRequest :: n -> n -> n -> NeilsenMizuno n ()
sendRequest dest source originator = liftF (SendRequest dest source originator ())

receiveRequest :: NeilsenMizuno n (n,n)
receiveRequest = liftF (ReceiveRequest id)

sendToken :: n -> NeilsenMizuno n ()
sendToken dest = liftF (SendToken dest ())

receiveToken :: NeilsenMizuno n ()
receiveToken = liftF (ReceiveToken ())

nonCriticalSection :: NeilsenMizuno n ()
nonCriticalSection = liftF (NonCriticalSection ())

criticalSection :: NeilsenMizuno n ()
criticalSection = liftF (CriticalSection ())

lock :: NeilsenMizuno n ()
lock = liftF (Lock ())

unlock :: NeilsenMizuno n ()
unlock = liftF (Unlock ())

-----------------------------------------------------------------------------------

{- One half of the Neilsen-Mizuno algorithm.
   Should be run concurrently with neilsenMizunoReceive below.
   
   To run, write an interpreter, e.g.
       interpret :: NeilsenMizuno n () -> IO ()   
-}

neilsenMizunoMain :: NeilsenMizuno n ()
neilsenMizunoMain =

    forever $ do

        nonCriticalSection

        lock
        holding <- isHolding
        when (not holding) $ do

            Just parent <- getParent
            myId <- getMyId
            sendRequest parent myId myId

            setParent Nothing
            receiveToken

        setHolding False
        unlock

        criticalSection

        lock
        deferred <- getDeferred
        case deferred of
            Just d -> do
                sendToken d
                setDeferred Nothing
            Nothing -> setHolding True
        unlock

-- The other half of the Neilsen-Mizuno algorithm
neilsenMizunoReceive :: NeilsenMizuno n ()
neilsenMizunoReceive =

    forever $ do

        (source, originator) <- receiveRequest

        lock
        
        mParent <- getParent
        case mParent of
            Nothing -> do
                
                holding <- isHolding
                if holding
                    then do
                        sendToken originator
                        setHolding False
                    else do
                        setDeferred (Just originator)

            Just parent -> do
                myId <- getMyId
                sendRequest parent myId originator

        setParent (Just source)
        unlock
