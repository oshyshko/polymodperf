module Main where

import           Control.DeepSeq       (NFData, force)
import           Control.Exception     (evaluate)
import           Control.Monad         (replicateM_, void)
import           Data.Array.IO         (IOUArray, getElems, newArray)
import           Data.Array.MArray     (MArray, readArray, writeArray)
import           Data.Ratio            (numerator)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import qualified SomeModule

-- $ ./scripts/build-exec.sh
-- Main.test
-- (11000000,2010)
-- (11000000,239)
-- (11000000,240)
-- (11000000,242)
-- (11000000,237)
--
-- SomeModule.test
-- (11000000,6376)
-- (11000000,4851)
-- (11000000,5455)
-- (11000000,5096)
-- (11000000,5206)
--
main :: IO ()
main = do
    putStrLn "Main.test"
    replicateM_ 5 $
        timeOf (run (Main.test (+ 1))) >>= print

    putStrLn ""

    putStrLn "SomeModule.test"
    replicateM_ 5 $
        timeOf (run (SomeModule.test (+ 1))) >>= print

-- same as SomeModule.test
test :: MArray a t m => (t -> t) -> a Int t -> m ()
test f a =
    mapM_ (\ xy -> do
              v <- get a xy
              set a xy (f v))

          [ (x,y) | y <- [0..SomeModule.side - 1],
                    x <- [0..SomeModule.side - 1],
                    n <- [0..10]]
  where
    get :: MArray a e m => a Int e -> (Int, Int) -> m e
    get a (x,y) = readArray a (x + y * SomeModule.side)

    set :: MArray a e m => a Int e -> (Int, Int) -> e -> m ()
    set a (x,y) = writeArray a (x + y * SomeModule.side)

-- utility fns
run :: (IOUArray Int Int -> IO a) -> IO Int
run f = do
    a <- newArray (0, SomeModule.side ^ 2 - 1) 0 :: IO (IOUArray Int Int)
    f a
    sum <$> getElems a

timeOf :: NFData a => IO a -> IO (a, Integer)
timeOf ioa = do
    start <- timeInMillis
    a <- force <$> ioa
    void $ evaluate a
    end <- timeInMillis
    pure (a, end - start)
  where
    timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
    timeInMillis = (`div` 1000) . fromIntegral <$> timeInMicros

