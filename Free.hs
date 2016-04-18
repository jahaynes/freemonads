{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Monad.Free   (Free (Free, Pure))
import Safe                 (atMay)


{- Declaration of our Collection DSL (as a free monad) -}
data CollectionFunctor a r = Create a (a -> r)
                           | Read Int (Maybe a -> r)
                           | Update Int a r
                           | Delete Int r
                           | GetAll ([a] -> r) deriving Functor


{- Short-hand for our free monad to make it look nicer -}
type Collection a r = Free (CollectionFunctor a) r


{- add a new item to the collection -}
add :: a -> Collection a a
add x = Free (Create x Pure)


{- (Maybe) retrieve an item (by index) from the collection -}
get :: Int -> Collection a (Maybe a)
get i = Free (Read i Pure)


{- Update an item (by index) from the collection -}
update :: Int -> a -> Collection a ()
update i x = Free (Update i x (Pure ()))


{- Delete an item (by index) from the collection -}
delete :: Int -> Collection a ()
delete i = Free (Delete i (Pure ()))


{- Return all items in the collection -}
getAll :: Collection a [a]
getAll = Free (GetAll Pure)


{- Pure, monadic interpreter for our Collection DSL -}
runPure :: Monad m => [a] -> Collection a b -> m b
runPure xs (Free (Create x next)) = runPure (xs++[x]) (next x)
runPure xs (Free (Read i next)) = runPure xs (next (atMay xs i))
runPure xs (Free (Update i x next)) = runPure (take i xs ++ x : drop (i+1) xs) next
runPure xs (Free (Delete i next)) = runPure (take i xs ++ drop (i+1) xs) next
runPure xs (Free (GetAll next)) = runPure xs (next xs)
runPure     _ (Pure r) = return r


{- Sample user program -}
myProgram :: Collection String [String]
myProgram = do
    mapM_ add ["one", "two", "three"] 
    update 1 ("four")
    delete 2
    getAll


main :: IO ()
main = do
    results <- runPure ["zero"] myProgram
    print results
