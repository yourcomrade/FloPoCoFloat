{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main  where
import Clash.Prelude
import FPFloat
import Test.QuickCheck
import qualified Data.Number.MPFR as M 
import qualified Prelude as P
import qualified Test.QuickCheck as TQ 

genrandomBitVector :: KnownNat n => Gen (BitVector n)
genrandomBitVector = arbitrarySizedBoundedIntegral
    

instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Arbitrary (FoFloat wE wF rnd) where 
    arbitrary :: Gen (FoFloat wE wF rnd)
    arbitrary = do
        ebv <- genrandomBitVector::Gen(BitVector wE)
        fbv <- genrandomBitVector::Gen(BitVector wF)
        signbv <- genrandomBitVector::Gen(BitVector 1)
        let vecres = (01::(BitVector 2)) ++# signbv ++# ebv ++# fbv 
        return( FoFloat vecres)

prop_checkconversion::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    P.Bool
prop_checkconversion x = (toFoFloat (toMPFR x):: (FoFloat wE wF rnd)) ==  x

prop_checkRational::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    P.Bool
prop_checkRational x = (P.fromRational (P.toRational x):: (FoFloat wE wF rnd)) == x
-- |This test should pass
--
main :: IO ()
main = do  
    print "Testing checkconversion for normal floating point numbers"
    TQ.quickCheck (prop_checkconversion @4 @11 @M.Near)
    print "Testing checkRational for normal floating point numbers"
    TQ.quickCheck (prop_checkRational @4 @11 @M.Near)