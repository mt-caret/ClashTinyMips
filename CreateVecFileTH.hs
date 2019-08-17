{-# Language AllowAmbiguousTypes #-}
module CreateVecFileTH (vecFromFile, listToVec, listToVecWithDefault) where

import qualified Prelude as P

import Clash.Prelude
import Clash.Explicit.BlockRam.File (initMem)
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH (ExpQ)

listToVecU :: Lift a => UNat n -> a -> [a] -> Vec n a
listToVecU UZero _ _ = Nil
listToVecU n x' [] = replicate (fromUNat n) x'
listToVecU (USucc s) x' (x:xs) = x :> (listToVecU s x' xs)

listToVecWithDefault :: Lift a => SNat n -> a -> [a] -> Vec n a
listToVecWithDefault = listToVecU . toUNat

listToVec :: (Lift a, Default a) => SNat n -> [a] -> Vec n a
listToVec n xs = listToVecWithDefault n def xs

vecFromFile :: (KnownNat height, KnownNat width) => FilePath -> Vec height (BitVector width)
vecFromFile filepath =
    unsafePerformIO $ withSNat listToVec <$> initMem filepath

--listToVec :: (KnownNat n, Lift a) => a -> [a] -> Vec n a
--listToVec = withSNat listToVec

--listToVecTH' :: Lift a => SNat n -> a -> [a] -> ExpQ
--listToVecTH' n _ _ | snatToInteger n == 0 = [| Nil |]
--listToVecTH' n def [] = [| def :> $(listToVecTH' (subSNat n d1) def []) |]
--listToVecTH' n def (x:xs) = [| x :> $(listToVecTH' (subSNat n d1) def xs) |]

--padList :: (KnownNat length, Num a) => [a] -> Vector length a
--padList xs = $(listToVecTH (xs <> (P.replicate (withSNat snatToNum - P.length xs) 0)))


initMem' :: KnownNat n => SNat n -> FilePath -> IO [BitVector n]
initMem' _ = initMem

vecFromFile' :: (KnownNat height, KnownNat width) => SNat height -> SNat width -> FilePath -> ExpQ
vecFromFile' height width filepath =
    listToVecTH $ contents <> (P.replicate (height' - P.length contents) 0)
  where
    height' = snatToNum height
    contents = unsafePerformIO $ initMem' width filepath

--vecFromFile :: KnownNat width => SNat width -> FilePath -> ExpQ
--vecFromFile width filepath =
--    listToVecTH . unsafePerformIO $ initMem' width filepath

--vecFromFile' :: KnownNat n => SNat n -> FilePath -> ExpQ
--vecFromFile' n filepath = (runIO $ (initMem' n filepath))
