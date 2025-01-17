{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module FPFloat  where
import qualified Data.Number.MPFR as M --import functions
-- import instances


import Clash.Prelude
import Data.Typeable ( Typeable)
import qualified Prelude as P
import qualified Data.Bits as DBits
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.Proxy


class KnownRnd (rnd :: M.RoundMode) where
    rndVal :: Proxy rnd -> M.RoundMode 

instance KnownRnd M.Up where
    rndVal _ = M.Up

instance KnownRnd M.Down where
    rndVal _ = M.Down

instance KnownRnd M.Near where
    rndVal _ = M.Near

instance KnownRnd M.Zero where
    rndVal _ = M.Zero


-- |FoFloat: floating point type for FloPoCo Floating type supports arbitrary floating point
-- which has 2 extra bits for specical cases. 
--  
-- __Example:__
--
-- > import qualified Data.Number.MPFR as M
-- > type SingleFloat = FoFloat 8 23 M.Near 
-- > -- 8: number of exponent bit
-- > -- 23: number of mantisa bit
-- > -- M.Near: Rounding mode (to nearest, tie to even)
--
newtype FoFloat (wE::Nat ) (wF::Nat) (rndMode:: M.RoundMode) = FoFloat{unFoFloat::BitVector(1 + 2 + wE + wF)}
    deriving(Generic, Typeable, Show, BitPack, Eq, NFDataX, ShowX, Lift)

-- | class showMPFR is to print out the floating point representation
-- 
class ShowMPFR a where
    showMPFR::a -> P.String

instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => ShowMPFR (FoFloat wE wF rnd) where
    showMPFR = P.show .toMPFR


-- |This function decomposes FoFloat into each component
--  
getComponents::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    (BitVector 2, BitVector 1, BitVector wE, BitVector wF)
getComponents (FoFloat bv) = let
    extV :: BitVector 2
    signV:: BitVector 1
    expV ::BitVector wE
    fracV::BitVector wF
    (extV, signV, expV, fracV) = unpack bv
    in (extV, signV, expV, fracV)

-- | This function is to get 2 extension bits from FoFloat
--
getExtV::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    BitVector 2
getExtV fofloat = let 
    (extV::BitVector 2,_,_,_) = getComponents fofloat
    in extV
-- |This function is to get sign bit from FoFloat
--
getSignV::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    BitVector 1
getSignV fofloat = let 
    (_, signV::BitVector 1, _, _) = getComponents fofloat
    in signV

-- |This function is to get exponent bits from FoFloat
--
getExpV::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    BitVector wE
getExpV fofloat = let
    (_,_,expV::BitVector wE,_) = getComponents fofloat
    in expV

-- |This function is to get mantisa bits from FoFloat
--
getFracV::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    BitVector wF 
getFracV fofloat = let
    (_,_,_,fracV::BitVector wF) = getComponents fofloat
    in fracV
-- |This function is to get round mode from FoFloat
-- 
getRoundMode::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    M.RoundMode
getRoundMode _ = rndVal (Proxy @rnd)

-- |This function is convert FoFloat into MPFR type for software simulation
--
toMPFR ::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd ->
    M.MPFR
toMPFR fofloat = do
    let (extVal::BitVector 2, signVal::BitVector 1, expBV::BitVector wE, fracBV::BitVector wF) = getComponents fofloat
    let wEVal::Int =  fromInteger (natVal (Proxy @wE))
    let wFVal =  fromInteger (natVal (Proxy @wF))
    let rndModeVal = getRoundMode fofloat
    let expVal::Integer = P.fromIntegral ( bitCoerce (expBV) ::Unsigned wE)
    let fracVal::Integer = P.fromIntegral (  bitCoerce (fracBV) ::Unsigned wF)
    case extVal of
        -- Zero case
        00 -> do
            if signVal == 0 then
                M.fromDouble rndModeVal (1 + wFVal) 0.0
            else
                M.fromDouble rndModeVal (1 + wFVal) (-0.0)
        -- Infinity case
        10 -> do
            if signVal == 0 then
                M.setInf (1 + wFVal) 1
            else
                M.setInf (1 + wFVal) (-1)
        -- NaN case
        11 -> do
            let val = M.setNaN (1 + wFVal)
            val
        -- Normal number case
        01 -> do
            -- mpfr_val = (-1)^(sign) * (1 + frac/2^(wF))*2^(unbiased_exp)
            -- unbiased_exp = exp - (shiftL 1 (wE - 1) ) + 1
            let precVal = 2 + wFVal
            let frac_mpfr = M.fromIntegerA rndModeVal precVal (toInteger fracVal)
            let temp = M.div2i rndModeVal precVal frac_mpfr (fromIntegral wFVal) -- (frac/2^(wF))
            
            let temp1 = M.addw rndModeVal precVal temp 1 -- (1 + frac/2^(wF))
            let unbiased_exp = expVal - (DBits.shiftL 1 (fromIntegral wEVal - 1)) + 1
            let mpfr_val = M.mul2i rndModeVal precVal temp1 (P.fromInteger unbiased_exp)
            if signVal == 1 then do
                let neg_val = M.neg rndModeVal precVal mpfr_val
                neg_val
            else do
                mpfr_val
{-# OPAQUE toMPFR #-}

-- | Function to convert MPFR type to FoFloat
--
toFoFloat :: 
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    M.MPFR ->  
    FoFloat wE wF rnd
toFoFloat num = do
    let rndValue = M.Near
    if M.isNaN num then 
        let extV::BitVector 2 = 11
            sign::BitVector 1 = 1
            expV::BitVector wE = 0
            fracV::BitVector wF = 0
            vecres = extV ++# sign ++# expV ++# fracV
        in
        FoFloat vecres:: FoFloat wE wF rndVal
    else if M.isInfinite num then 
        let extV::BitVector 2 = 10
            sign::BitVector 1 = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1
            expV::BitVector wE = 0
            fracV::BitVector wF = 0
            vecres = extV ++# sign ++# expV ++# fracV
        in
        FoFloat vecres:: FoFloat wE wF rndVal
    else if M.isZero num then
        let extV::BitVector 2 = 00
            sign::BitVector 1 = if M.signbit num == P.False then 0 else 1
            expV::BitVector wE = 0
            fracV::BitVector wF = 0
            vecres = extV ++# sign ++# expV ++# fracV
        in
        FoFloat vecres:: FoFloat wE wF rndVal
        
    else do
        -- Get exponent
		-- getExp return exponent for significant in [1/2,1)
		-- but we require [1,2). Hence the -1.
        let expVal :: Int = fromIntegral (M.getExp num - 1)
        let wFVal ::Int = fromInteger (natVal (Proxy @wF))
        let wEVal ::Int = fromInteger (natVal (Proxy @wE))
        -- Extract fraction 
        let precVal = M.getPrec num
        let absVal = M.absD rndValue precVal num
        let temp = M.div2i rndValue precVal absVal expVal
        let temp1 = M.subw rndValue precVal temp 1
        let temp2 = M.mul2i rndValue precVal temp1 wFVal
        let tempfracVal = M.toInt rndValue temp2
        -- Due to rounding, the fraction might overflow (i.e. become bigger
        -- than we expect).
        let valcomp::Int = DBits.shiftL 1 wFVal
        if tempfracVal == valcomp then 
            let biasedExp = expVal + 1 + ((DBits.shiftL 1 (wEVal - 1)) - 1) 
                extV::BitVector 2 = 01
                sign::BitVector 1 = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1
                expV::BitVector wE = P.fromIntegral biasedExp
                fracV::BitVector wF = 0
                vecres = extV ++# sign ++# expV ++# fracV
            in
            FoFloat vecres :: FoFloat wE wF rndVal
        else if tempfracVal > valcomp then
            error "Fraction is too big after conversion"
        else if tempfracVal < 0 then
            error "Fraction is negative after conversion"
        else do
            let biasedExp = expVal + ((DBits.shiftL 1 (wEVal - 1)) - 1)
            -- Handle underflow
            if biasedExp < 0 then 
                let extV::BitVector 2 = 00
                    sign::BitVector 1 = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1
                    expV::BitVector wE = 0
                    fracV::BitVector wF = 0
                    vecres = extV ++# sign ++# expV ++# fracV
                in
                FoFloat vecres :: FoFloat wE wF rndVal
            -- Handle overflow
            else if biasedExp >= (DBits.shiftL 1 wEVal) then
                let extV::BitVector 2 = 10
                    sign::BitVector 1 = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1
                    expV::BitVector wE = 0
                    fracV::BitVector wF = 0
                    vecres = extV ++# sign ++# expV ++# fracV
                in
                FoFloat vecres :: FoFloat wE wF rndVal
            else
                let extV::BitVector 2 = 01
                    sign::BitVector 1 = if (fromMaybe 0 (M.sgn num)) > 0 then 0 else 1
                    expV::BitVector wE = P.fromIntegral biasedExp
                    fracV::BitVector wF = P.fromIntegral tempfracVal
                    vecres = extV ++# sign ++# expV ++# fracV
                in
                FoFloat vecres :: FoFloat wE wF rndVal
                
{-# OPAQUE toFoFloat #-}            
-- | Function to get number of precision bits
--
getPrecision::
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd-> 
    M.Precision
getPrecision _ = fromInteger (natVal (Proxy @wE)) + fromInteger (natVal (Proxy @wF))
-- |Function to get the maximum number of precision bits between 2 different FoFloat types
--
maxPrecision::
    forall wE wF rnd wE1 wF1 rnd1.
    (KnownNat wE, KnownNat wF, KnownRnd rnd, KnownNat wE1, KnownNat wF1, KnownRnd rnd1) =>
    FoFloat wE wF rnd -> 
    FoFloat wE1 wF1 rnd1 -> 
    M.Precision
maxPrecision fo1 fo2 = do
    let a = getPrecision fo1 
    let b =  getPrecision fo2
    P.max a b
-- |Function to get the minimum number of precision bits between 2 different FoFloat types
--
minPrecision::
    forall wE wF rnd wE1 wF1 rnd1.
    (KnownNat wE, KnownNat wF, KnownRnd rnd, KnownNat wE1, KnownNat wF1, KnownRnd rnd1) =>
    FoFloat wE wF rnd -> 
    FoFloat wE1 wF1 rnd1 -> 
    M.Precision
minPrecision fo1 fo2 = do
    let a = getPrecision fo1 
    let b =  getPrecision fo2
    P.min a b

-- | Funtion to get the max (number of exponent bit, number of mantisa bit) between 2 different FoFloat types
--
getwEwFfromMaxPrec:: forall wE wF rnd wE1 wF1 rnd1.
    (KnownNat wE, KnownNat wF, KnownRnd rnd, KnownNat wE1, KnownNat wF1, KnownRnd rnd1) =>
    FoFloat wE wF rnd -> 
    FoFloat wE1 wF1 rnd1 -> 
    (Int, Int)
getwEwFfromMaxPrec fo1 fo2 = do
    if(getPrecision fo1 == maxPrecision fo1 fo2) then
        (fromInteger (natVal (Proxy @wE)), fromInteger (natVal  (Proxy @wF)))
    else
        (fromInteger (natVal (Proxy @wE1)), fromInteger (natVal  (Proxy @wF1)))
-- | Funtion to get the min (number of exponent bit, number of mantisa bit) between 2 different FoFloat types
--
getwEwFfromMinPrec:: forall wE wF rnd wE1 wF1 rnd1.
    (KnownNat wE, KnownNat wF, KnownRnd rnd, KnownNat wE1, KnownNat wF1, KnownRnd rnd1) =>
    FoFloat wE wF rnd -> 
    FoFloat wE1 wF1 rnd1 ->  
    (Int, Int) 
getwEwFfromMinPrec fo1 fo2 = do
    if(getPrecision fo1 == minPrecision fo1 fo2) then
        (fromInteger (natVal (Proxy @wE)), fromInteger (natVal  (Proxy @wF)))
    else
        (fromInteger (natVal (Proxy @wE1)), fromInteger (natVal  (Proxy @wF1)))


instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Num (FoFloat wE wF rnd) where
    f1 + f2        = do
        let m1 = toMPFR f1
        let m2 = toMPFR f2
        let temp = M.add (rndVal (Proxy @rnd)) (getPrecision f1) m1 m2
        toFoFloat temp 
    f1 - f2        = do
        let m1 = toMPFR f1
        let m2 = toMPFR f2
        let temp = M.sub (rndVal (Proxy @rnd)) (maxPrecision f1 f2) m1 m2
        toFoFloat temp 
        
    f1 * f2        = do 
        let m1 = toMPFR f1
        let m2 = toMPFR f2
        let temp = M.mul (rndVal (Proxy @rnd)) (maxPrecision f1 f2) m1 m2
        toFoFloat temp 
    negate f1      = do
        let m1 = toMPFR f1
        let temp = M.neg (rndVal (Proxy @rnd)) (getPrecision f1) m1
        toFoFloat temp 
    abs f1         = do
        let m1 = toMPFR f1
        let temp = M.absD (rndVal (Proxy @rnd)) (getPrecision f1) m1
        toFoFloat temp 
    signum f1     = 
        let extV = getExtV f1 in
        if extV == 00 then 0
        else if extV == 11 then f1
        -- Infinity case and Normal case
        else 
            if(getSignV f1) == 0 then 1 else 0
            

    fromInteger i = toFoFloat (M.fromIntegerA (rndVal (Proxy @rnd)) (fromInteger (natVal (Proxy @wE)) + fromInteger (natVal (Proxy @wF))) i) 
        
-- | Helper function for toRational function 
-- which allows convert FoFloat to Rational number
-- 
decomposeFoFloat:: 
    forall wE wF rnd .
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd->
    (Integer, Integer)
decomposeFoFloat fofloat  
    | getExtV fofloat == 00 = (0,0)
    |otherwise = let
    
    fracVal = toInteger (getFracV fofloat)
    temp_unbiased_exp = toInteger (getExpV fofloat) - (DBits.shiftL 1 (fromInteger (natVal (Proxy @wE)) - 1)) + 1 
    unbiased_exp = if temp_unbiased_exp >= 0 then temp_unbiased_exp else 0
    temp_num = toInteger ((DBits.shiftL 1 (fromInteger (natVal (Proxy @wF)))) + fracVal) * (DBits.shiftL 1 (fromInteger unbiased_exp))
    num = if getSignV fofloat == 1 then -temp_num else temp_num
    dem = if temp_unbiased_exp >= 0 then
        natVal (Proxy @wF)
        else
            (natVal (Proxy @wF)) - temp_unbiased_exp
    in

    (num,-dem)

cmpFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd -> 
    P.Ordering
cmpFoFloat fo1 fo2 = do
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    P.compare m1 m2 

lessFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd ->  
    P.Bool
lessFoFloat fo1 fo2 = do 
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    M.less m1 m2
lesseqFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd ->  
    P.Bool
lesseqFoFloat fo1 fo2 = do 
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    M.lesseq m1 m2
greaterFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd ->  
    P.Bool
greaterFoFloat fo1 fo2 = do 
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    M.greater m1 m2
greatereqFoFloat :: 
    forall wE wF rnd.
    (KnownNat wE, KnownNat wF, KnownRnd rnd) =>
    FoFloat wE wF rnd -> 
    FoFloat wE wF rnd ->  
    P.Bool
greatereqFoFloat fo1 fo2 = do 
    let m1 = toMPFR fo1 
    let m2 = toMPFR fo2 
    M.greatereq m1 m2



instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => Ord (FoFloat wE wF rnd) where
    compare  = cmpFoFloat 
    (<)       = lessFoFloat
    (<=)      = lesseqFoFloat
    (>)       = greaterFoFloat
    (>=)      = greatereqFoFloat
instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Real (FoFloat wE wF rnd) where
    toRational d = n % (2  P.^ e)
        where (n', e') = decomposeFoFloat d
              (n, e) = if e' >= 0 then ((n' * (2 P.^ e')), 0)
                         else (n', - e')
instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => Fractional (FoFloat wE wF rnd) where
    f1 / f2        = do
        let m1 = toMPFR f1
        let m2 = toMPFR f2
        let temp = M.div (rndVal (Proxy @rnd)) (maxPrecision f1 f2) m1 m2
        
        toFoFloat temp 
    fromRational r = do
        let n = M.fromIntegerA M.Near (fromInteger (natVal (Proxy @(wF + 1)))) (P.fromInteger (numerator r))
        let d = M.fromIntegerA M.Near (fromInteger (natVal (Proxy @(wF + 1)))) (P.fromInteger (denominator r))
        let temp = M.div M.Near (fromInteger (natVal (Proxy @(wF + 1)))) n d
        toFoFloat temp 

    recip d        = do
        let one = M.fromWord M.Near (fromInteger (natVal (Proxy @wF))) 1
        let denom = toMPFR d 
        let temp = M.div (rndVal (Proxy @rnd)) (getPrecision d) one denom 
        toFoFloat temp 



instance (KnownNat wE, KnownNat wF, KnownRnd rnd) => Floating (FoFloat wE wF rnd) where
    pi           = toFoFloat (M.pi M.Near (fromInteger (natVal (Proxy @wF)))) 
    exp d        = do 
        let temp = toMPFR d 
        let tempres = M.exp M.Faithful (getPrecision d) temp 
        toFoFloat tempres 
    log d        = do 
        let temp = toMPFR d 
        let tempres = M.log M.Faithful (getPrecision d) temp 
        let res = toFoFloat tempres 
        res 
    sqrt d       = do 
        let temp = toMPFR d 
        let tempres = M.sqrt M.Near (getPrecision d) temp 
        toFoFloat tempres 
    (**) f1 f2    = do 
        let temp = toMPFR f1 
        let temp1 = toMPFR f2
        let tempres = M.pow M.Faithful (getPrecision f1) temp temp1
        toFoFloat tempres 
    logBase _ _ = error "logBase is not defined for FoFloat"
    sin _        = error "sin is not defined for FoFloat"
    cos _        = error "cos is not defined for FoFloat"
    tan _        = error "tan is not defined for FoFloat"
    asin _       = error "asin is not defined for FoFloat"
    acos _       = error "acos is not defined for FoFloat"
    atan _       = error "atan is not defined for FoFloat"
    sinh _       = error "sinh is not defined for FoFloat"
    cosh _       = error "cosh is not defined for FoFloat"
    tanh _       = error "tanh is not defined for FoFloat"
    asinh _      = error "asinh is not defined for FoFloat"
    acosh _      = error "acosh is not defined for FoFloat"
    atanh _      = error "atanh is not defined for FoFloat"

instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => RealFrac (FoFloat wE wF rnd) where
    properFraction _ = error "properFraction is not defined for FoFloat"

instance  (KnownNat wE, KnownNat wF, KnownRnd rnd) => RealFloat (FoFloat wE wF rnd) where
    floatRadix _ = 2
    floatDigits = fromInteger . toInteger . getPrecision
    floatRange _ = error "floatRange is not defined for FoFloat numbers"
    decodeFloat _ = error "decodeFloat is not defined for FoFloat numbers"
    encodeFloat _ _ = error "encodeFloat function is not defined for FoFloat"
      
    isNaN fo = if (getExtV fo) == 11 then P.True else P.False
    isInfinite fo = if (getExtV fo) == 10 then P.True else P.False
    
    isDenormalized _ = False
    isNegativeZero fo = if (getExtV fo) == 00 && (getSignV fo) == 1 then P.True else P.False
    isIEEE _ = False
    atan2 _ _ = error "atan2 is not defined for FoFloat" 


