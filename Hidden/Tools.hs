--	Tools.hs
--
--	Author: Yoshikuni Jujo <PAF01143@nifty.ne.jp>
--

module Hidden.Tools (
  isSymbol
, modifyFst
, modifySnd
, first
, second
, third
, modifyFirst
, modifySecond
, modifyThird
, guardEqual
, (|||)
, (&&&)
, isBit7On
-- , bifurcate
-- , cat2funcL
, skipRet
, (>..>)
, ignoreCase
, ifM
, applyIf
, headOrErr
) where

import Data.Char          ( ord, toUpper, toLower )
import Data.Bits          ( (.&.), shiftL )
import Control.Monad      ( MonadPlus, guard )

isSymbol :: Char -> Bool
isSymbol = flip elem "!\"#$%&'()*+,-./:;<=>?@[\\]^_'{|}~"

modifyFst :: (a -> c) -> (a, b) -> (c, b)
modifyFst f (x, y) = (f x, y)
modifySnd :: (b -> c) -> (a, b) -> (a, c)
modifySnd f (x, y) = (x, f y)

guardEqual :: (MonadPlus m, Eq a) => m a -> m a -> m ()
guardEqual m1 m2 = do { x <- m1; y <- m2; guard (x == y) }

first  :: (a, b, c) -> a
first  (x, _, _) = x
second :: (a, b, c) -> b
second (_, y, _) = y
third  :: (a, b, c) -> c
third  (_, _, z) = z

modifyFirst  :: (a -> d) -> (a, b, c) -> (d, b, c)
modifyFirst  f (x, y, z) = (f x, y, z)
modifySecond :: (b -> d) -> (a, b, c) -> (a, d, c)
modifySecond f (x, y, z) = (x, f y, z)
modifyThird  :: (c -> d) -> (a, b, c) -> (a, b, d)
modifyThird  f (x, y, z) = (x, y, f z)

(|||),(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(p1 ||| p2) x = p1 x || p2 x
(p1 &&& p2) x = p1 x && p2 x

isBit7On :: Char -> Bool
isBit7On c = ord c .&. shiftL 1 7 /= 0

{-
bifurcate :: (a -> a -> b) -> a -> b
bifurcate f x = f x x

cat2funcL :: (a -> c) -> (b -> c) -> a -> b -> [c]
cat2funcL f g x y =  [f x, g y ]
-}

skipRet :: Monad m => m b -> a -> m a
skipRet p x = p >> return x

(>..>) :: Monad m => m a -> m b -> m (a, b)
m1 >..> m2 = do { x <- m1; y <- m2; return (x, y) }

ignoreCase :: (Char -> Bool) -> Char -> Bool
ignoreCase p c = p (toLower c) || p (toUpper c)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p mt me = do b <- p
                 if b then mt
		      else me

applyIf :: Bool -> (a -> a) -> a -> a
applyIf True f  = f
applyIf False _ = id

headOrErr :: String -> [a] -> a
headOrErr err []    = error err
headOrErr _   (x:_) = x
