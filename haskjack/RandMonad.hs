
module RandMonad
( Ranad
, applyGen
, shuffle
) where

import System.Random (RandomGen, randomR)

data Ranad g a = Ranad (g -> (a, g))

instance (RandomGen g) => Functor (Ranad g) where
    -- fmap :: (a -> b) -> Ranad g a -> Rand g b
    fmap f (Ranad randStep) = Ranad $ \rgen -> let (x, rgen') = randStep rgen
                                               in (f x, rgen')

instance (RandomGen g) => Applicative (Ranad g) where
    -- pure :: a -> f a
    pure x = Ranad (\rgen -> (x, rgen))
    -- (<*>) :: f (a -> b) -> f a -> f b
    (Ranad genF) <*> (Ranad genX) =
        Ranad $ \rgen -> let (f, rgen') = genF rgen
                             (x, rgen'') = genX rgen'
                         in (f x, rgen'')

instance (RandomGen g) => Monad (Ranad g) where
    return = pure
    (Ranad genX) >>= getRb = Ranad $ \rgen ->
        let (x, rgen') = genX rgen
            (Ranad genY) = getRb x
        in genY rgen'

applyGen :: (RandomGen g) => g -> Ranad g a -> (a, g)
applyGen rgen (Ranad getX) = getX rgen

shuffle :: (RandomGen g) => [a] -> Ranad g [a]
shuffle items = Ranad $
    let shuffle' [] g = ([], g)
        shuffle' xs g =
            let (i, g') = randomR (0, length xs) g
                (ys, zs) = splitAt (i `mod` length xs) xs
                (zs', g'') = shuffle' (ys ++ drop 1 zs) g'
            in (head zs : zs', g'')
    in shuffle' items
