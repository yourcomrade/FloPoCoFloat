{-# LANGUAGE MagicHash, CPP #-}
{-|
    Module      :  Data.Number.MPFR.Instances.Faithful
    Description :  Instance declarations
    Copyright   :  (c) yourcomrade
    License     :  BSD3

    Maintainer  :  minhxecole@gmail.com
    Stability   :  experimental
    Portability :  non-portable

  This module defines instances 'Num', 'Real', 'Fractional', 'Floating' and 'RealFrac' of 'MPFR'.
  Operations are rounded with 'RoundMode' 'Faithful' and computed with maximum precision of two
  operands or with the precision of the operand.
-}

module Data.Number.MPFR.Instances.Faithful ()
where

import qualified Data.Number.MPFR.Arithmetic as A
import qualified Data.Number.MPFR.Special as S
import Data.Number.MPFR.Misc
import Data.Number.MPFR.Assignment
import Data.Number.MPFR.Comparison
import Data.Number.MPFR.Internal
import Data.Number.MPFR.Conversion
import Data.Number.MPFR.Integer

import Data.Maybe

import Data.Ratio

import Prelude

-- #ifdef INTEGER_SIMPLE
-- --import GHC.Integer.Simple.Internals
-- #endif
-- #ifdef INTEGER_GMP
-- import GHC.Integer.GMP.Internals
-- import qualified GHC.Exts as E
-- #endif

instance Num MPFR where
    d + d'        = A.add Faithful (maxPrec d d') d d'
    d - d'        = A.sub Faithful (maxPrec d d') d d'
    d * d'        = A.mul Faithful (maxPrec d d') d d'
    negate d      = A.neg Faithful (getPrec d) d
    abs d         = A.absD Faithful (getPrec d) d
    signum        = fromInt Faithful minPrec . fromMaybe (-1) . sgn
    fromInteger i =
        fromIntegerA Faithful (max minPrec $ 1 + bitsInInteger i) i
-- #ifdef INTEGER_SIMPLE
--     fromInteger i =
--         fromIntegerA Faithful (max minPrec $ 1 + bitsInInteger i) i
-- #endif
-- #ifdef INTEGER_GMP
--     fromInteger (S# i) = fromInt Faithful minPrec (E.I# i)
--     fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral . abs $ E.I# n * bitsPerIntegerLimb) i
-- #endif

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = A.div Up (maxPrec d d') d d'
    fromRational r = fromInteger n / fromInteger d
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = S.pi Faithful 53
    exp d        = S.exp Faithful (getPrec d) d
    log d        = S.log Faithful (getPrec d) d
    sqrt d       = A.sqrt Faithful (getPrec d) d
    (**) d d'    = A.pow Faithful (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = S.sin Faithful (getPrec d) d
    cos d        = S.cos Faithful (getPrec d) d
    tan d        = S.tan Faithful (getPrec d) d
    asin d       = S.asin Faithful (getPrec d) d
    acos d       = S.acos Faithful (getPrec d) d
    atan d       = S.atan Faithful (getPrec d) d
    sinh d       = S.sinh Faithful (getPrec d) d
    cosh d       = S.cosh Faithful (getPrec d) d
    tanh d       = S.tanh Faithful (getPrec d) d
    asinh d      = S.asinh Faithful (getPrec d) d
    acosh d      = S.acosh Faithful (getPrec d) d
    atanh d      = S.atanh Faithful (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Faithful (getPrec d) d

instance RealFloat MPFR where
    floatRadix _ = 2
    floatDigits = fromInteger . toInteger . getPrec
    floatRange _ = error "floatRange is not defined for MPFR numbers"
    decodeFloat x = (d,e)
      where
      (d,eE) = decompose x
      e = fromInteger (toInteger eE)
    encodeFloat d e =
      (fromInteger d) / ((fromInteger 2)^e) -- TODO: construct it directly
    isNaN (MP _ _ e _) = (e == expNaN)
    isInfinite (MP _ _ e _) = (e == expInf)
    isDenormalized _ = False
    isNegativeZero d@(MP _ _ e _) = (e == expZero && signbit d)
    isIEEE _ = False
    atan2 d1 d2 = S.atan2 Faithful (maxPrec d1 d2) d1 d2
