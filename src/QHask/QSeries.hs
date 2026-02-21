module QHask.QSeries
    ( QSeries(..)
    , zeroQS
    , oneQS
    , qPower
    , addQS
    , subQS
    , negQS
    , scaleQS
    , mulQS
    , qFactorial
    , qPochhammer
    , qBinomial
    , inverseQS
    , takeQS
    ) where

-- | A formal power series in q, represented as a lazy infinite list.
-- The coefficient of q^k is at index k.
newtype QSeries = QSeries { coeffs :: [Integer] }

instance Show QSeries where
    show (QSeries cs) = "QSeries " ++ show (take 10 cs) ++ "..."

-- | The zero power series.
zeroQS :: QSeries
zeroQS = QSeries (repeat 0)

-- | The multiplicative identity: 1 + 0*q + 0*q^2 + ...
oneQS :: QSeries
oneQS = QSeries (1 : repeat 0)

-- | q^n as a power series.
qPower :: Int -> QSeries
qPower n
    | n < 0     = error "qPower: negative exponent"
    | otherwise = QSeries (replicate n 0 ++ 1 : repeat 0)

-- | Pointwise addition.
addQS :: QSeries -> QSeries -> QSeries
addQS (QSeries as) (QSeries bs) = QSeries (zipWith (+) as bs)

-- | Pointwise subtraction.
subQS :: QSeries -> QSeries -> QSeries
subQS (QSeries as) (QSeries bs) = QSeries (zipWith (-) as bs)

-- | Negate all coefficients.
negQS :: QSeries -> QSeries
negQS (QSeries as) = QSeries (map negate as)

-- | Scalar multiplication.
scaleQS :: Integer -> QSeries -> QSeries
scaleQS c (QSeries as) = QSeries (map (c *) as)

-- | Power series multiplication using McIlroy's streaming convolution.
--
-- (a0 + q*A') * B = a0*B + q*(A' * B)
--
-- This is O(n^2) to compute the first n coefficients, but streams lazily.
mulQS :: QSeries -> QSeries -> QSeries
mulQS (QSeries as) (QSeries bs) = QSeries (go as bs)
  where
    go [] _ = repeat 0
    go _ [] = repeat 0
    go (a:as') bss@(_:_) =
        zipWith (+) (map (a *) bss) (0 : go as' bss)

-- | (q)_n = prod_{k=1}^{n} (1 - q^k).
-- This is a finite polynomial (degree n*(n+1)/2) padded with zeros.
qFactorial :: Int -> QSeries
qFactorial n
    | n <= 0    = oneQS
    | otherwise = foldl mulQS oneQS [oneMinus k | k <- [1..n]]

-- | (q^a; q^m)_inf = prod_{k>=0} (1 - q^{a + k*m}), truncated to precision prec.
--
-- The infinite product is computed as a finite left fold: only factors with
-- exponent < prec affect coefficients below prec. The result is padded with
-- zeros beyond the precision bound.
qPochhammer :: Int -> Int -> Int -> QSeries
qPochhammer a m prec
    | a <= 0 || m <= 0 = error "qPochhammer: need a > 0 and m > 0"
    | otherwise = foldl mulQS oneQS factors
  where
    exponents = takeWhile (< prec) [a + k * m | k <- [0..]]
    factors   = map oneMinus exponents

-- | Gaussian q-binomial coefficient [n choose k]_q.
-- Computed as prod_{i=1}^{k} (1 - q^{n-k+i}) / (1 - q^i).
qBinomial :: Int -> Int -> QSeries
qBinomial n k
    | k < 0 || k > n = zeroQS
    | k == 0          = oneQS
    | otherwise       = foldl mulQS oneQS
        [ mulQS (oneMinus (n - k + i)) (inverseQS (oneMinus i))
        | i <- [1..k]
        ]

-- | Multiplicative inverse 1/f(q) as a lazy power series.
-- Requires f(0) = 1 or f(0) = -1 (since we work over Integer, not Rational).
--
-- For f = 1 + f1*q + f2*q^2 + ..., the inverse g satisfies:
-- g_0 = 1, g_n = -(f_1*g_{n-1} + f_2*g_{n-2} + ... + f_n*g_0)
inverseQS :: QSeries -> QSeries
inverseQS (QSeries (f0:fs))
    | f0 == 1   = QSeries gs
    | f0 == -1  = negQS (inverseQS (negQS (QSeries (f0:fs))))
    | otherwise = error "inverseQS: leading coefficient must be +/-1"
  where
    gs = 1 : [negate (sum [fs !! (k-1) * gs !! (n-k) | k <- [1..n]]) | n <- [1..]]
inverseQS (QSeries []) = error "inverseQS: empty series"

-- | Take the first n coefficients (indices 0..n-1).
takeQS :: Int -> QSeries -> [Integer]
takeQS n (QSeries cs) = take n cs

-- Helper: (1 - q^e) as a power series.
oneMinus :: Int -> QSeries
oneMinus e = QSeries (replicate e 0 ++ [-1] ++ repeat 0) `addQS` oneQS
