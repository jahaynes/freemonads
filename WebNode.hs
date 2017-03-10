{-# LANGUAGE OverloadedStrings #-}

-- Helper functions for the WebNode implementation

module WebNode where

import Control.Concurrent 
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.ByteString.Char8            (ByteString, pack, unpack)
import Data.IORef                       (IORef, newIORef, readIORef, writeIORef)
import Network.HTTP.Conduit             
import Network.HTTP.Types               (urlEncode, urlDecode)
import Web.Scotty                       (scotty, get, param, html)

type Ip = String

type Port = Int

data Req = Req Address Address 

newtype Address = Address String deriving (Show, Read, Eq)

data WebNode = WebNode
            { getMyId       :: Address
            , getReqChan    :: Chan Req
            , getTokenChan  :: Chan ()
            , getParent'    :: IORef (Maybe Address)
            , getDeferred'  :: IORef (Maybe Address)
            , getHolding'   :: IORef Bool
            , getLock       :: MVar ()
            , getCount      :: IORef Int
            , getManager    :: Manager
            }

makeNodes :: Ip -> Maybe Address -> [Port] -> IO [WebNode]
makeNodes mainIp _mParent _ps = do
    man <- newManager tlsManagerSettings    
    go man [] _mParent _ps
    where
    go   _ nodes      _      [] = return nodes
    go man nodes mParent (p:ps) = do
        a <- makeWebNode man mainIp p mParent
        go man (a:nodes) (Just (getMyId a)) ps    
    
    makeWebNode :: Manager -> Ip -> Port -> Maybe Address -> IO WebNode
    makeWebNode man myIp prt mParent = do

        reqChan <- newChan
        tokenChan <- newChan

        _ <- forkIO $ scotty prt $ do

            get "/msg" $ do
                req <- (Req <$> (decodeAddress <$> param "source")
                            <*> (decodeAddress <$> param "originator"))
                liftIO $ writeChan reqChan req
                html "OK"

            get "/tok" $ do
                liftIO $ writeChan tokenChan ()
                html "OK"

        WebNode <$> pure (Address (myIp++':':show prt))
                <*> pure reqChan
                <*> pure tokenChan
                <*> newIORef mParent
                <*> newIORef Nothing
                <*> newIORef (mParent == Nothing)
                <*> newEmptyMVar
                <*> newIORef 0
                <*> pure man
                
takeLock :: WebNode -> IO ()
takeLock node = putMVar (getLock node) ()

releaseLock :: WebNode -> IO ()
releaseLock node = takeMVar (getLock node)

isHolding :: WebNode -> IO Bool
isHolding node = readIORef (getHolding' node)

setHolding :: WebNode -> Bool -> IO ()
setHolding node holding = writeIORef (getHolding' node) holding

receiveToken :: WebNode -> IO ()
receiveToken node = readChan (getTokenChan node)

getParent :: WebNode -> IO (Maybe Address)
getParent node = readIORef (getParent' node)

setParent :: WebNode -> Maybe Address -> IO ()
setParent node parent = writeIORef (getParent' node) parent

getDeferred :: WebNode -> IO (Maybe Address)
getDeferred node = readIORef (getDeferred' node)

setDeferred :: WebNode -> Maybe Address -> IO ()
setDeferred node deferred = writeIORef (getDeferred' node) deferred
    
doNonCriticalSectionStuff :: Monad m => m ()
doNonCriticalSectionStuff = return ()

doCriticalSectionStuff :: WebNode -> IO ()
doCriticalSectionStuff node = do
    putStrLn $ show (getMyId node) ++ " entering critical section"
    x <- readIORef (getCount node)
    print x
    writeIORef (getCount node) (x+1)
    threadDelay 10000
    putStrLn $ show (getMyId node) ++ " leaving critical section"
    putStrLn ""

sendRequest :: WebNode -> Address -> Address -> Address -> IO ()
sendRequest node (Address dest) source origin =
    runResourceT $ do
        req <-  setQueryString [("source", Just (encodeAddress source))
                               ,("originator", Just (encodeAddress origin))]
            <$> parseRequest (concat ["http://", dest, "/msg"])
        "OK" <- responseBody <$> httpLbs req (getManager node)
        return ()

receiveRequest :: WebNode -> IO Req
receiveRequest node = readChan (getReqChan node)
        
sendToken :: WebNode -> Address -> IO ()
sendToken node (Address dest) =
    runResourceT $ do
        req <- parseRequest url
        "OK" <- responseBody <$> httpLbs req (getManager node)
        return ()
    where
    url = concat ["http://", dest, "/tok"]

encodeAddress :: Address -> ByteString
encodeAddress = urlEncode False . pack . show
    
decodeAddress :: ByteString -> Address
decodeAddress = read . unpack . urlDecode False
