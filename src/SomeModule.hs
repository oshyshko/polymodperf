module SomeModule where

import           Data.Array.MArray (MArray, readArray, writeArray)

side :: Int
side = 1000

-- same as Main.test
test :: MArray a t m => (t -> t) -> a Int t -> m ()
test f a =
    mapM_ (\ xy -> do
              v <- get a xy
              set a xy (f v))

          [ (x,y) | y <- [0..side-1],
                    x <- [0..side-1],
                    n <- [0..10]]
  where
    get :: MArray a e m => a Int e -> (Int, Int) -> m e
    get a (x,y) = readArray a (x + y * side)

    set :: MArray a e m => a Int e -> (Int, Int) -> e -> m ()
    set a (x,y) = writeArray a (x + y * side)
