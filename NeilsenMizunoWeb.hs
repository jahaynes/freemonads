import NeilsenMizuno
import WebNode

import Control.Concurrent               (threadDelay)
import Control.Concurrent.Async         (Async, async, waitAny)
import Control.Monad.Free               (Free (..))
import Network.Info                     (getNetworkInterfaces, ipv4, name, NetworkInterface)
import System.Environment               (getArgs)

main :: IO ()
main = do

    (port, mParent) <- parseArgs <$> getArgs

    --Find an ipv4 address which outside users might be able to connect to
    mainIp <- getMainIp
    
    --Spin up 10 nodes on the given IP and port-range
    nodes <- makeNodes mainIp mParent [port..port+10]

    --Wait for the nodes to spin up
    threadDelay 500000

    --If any of the nodes finish, terminate in error
    _ <- waitAny =<< concat <$> mapM runNode nodes
    error "Shouldn't terminate"

parseArgs :: [String] -> (Port, Maybe Address)
parseArgs [port] = (read port, Nothing)
parseArgs [port, parent] = (read port, Just (Address parent))
parseArgs _ = error "Usage: port | port parentAddress"

getMainIp :: IO Ip           
getMainIp = show . ipv4 . head
          . filter ipFilter <$> getNetworkInterfaces

    where
    ipFilter :: NetworkInterface -> Bool
    ipFilter ip = name ip /= "lo"
                && (show . ipv4 $ ip) /= "0.0.0.0"
                && (take 3 . show . ipv4 $ ip) /= "127"

runNode :: WebNode -> IO [Async ()]
runNode node = do
    a <- async $ runIO node neilsenMizunoMain
    b <- async $ runIO node neilsenMizunoReceive
    return [a,b]

runIO :: WebNode -> NeilsenMizuno Address () -> IO ()
runIO node = go

    where
    go :: NeilsenMizuno Address a -> IO a

    go (Pure x) = return x

    go (Free (GetMyId f)) = do
        let myId = getMyId node 
        go (f myId)

    go (Free (SendRequest dest source origin a)) = do
        sendRequest node dest source origin
        go a

    go (Free (ReceiveRequest f)) = do
        Req source originator <- receiveRequest node
        go (f (source,originator))

    go (Free (GetParent f)) = do
        parent <- getParent node
        go (f parent)

    go (Free (SetParent p a)) = do
        setParent node p
        go a

    go (Free (GetDeferred f)) = do
        deferred <- getDeferred node
        go (f deferred)

    go (Free (SetDeferred deferred a)) = do
        setDeferred node deferred
        go a

    go (Free (IsHolding f)) = do
        holding <- isHolding node
        go (f holding)

    go (Free (SetHolding holding a)) = do
        setHolding node holding
        go a

    go (Free (SendToken destination a)) = do        
        sendToken node destination
        go a

    go (Free (ReceiveToken a)) = do
        receiveToken node
        go a

    go (Free (NonCriticalSection a)) = do
        doNonCriticalSectionStuff
        go a

    go (Free (CriticalSection a)) = do
        doCriticalSectionStuff node
        go a

    go (Free (Lock a)) = do
        takeLock node
        go a

    go (Free (Unlock a)) = do
        releaseLock node
        go a
