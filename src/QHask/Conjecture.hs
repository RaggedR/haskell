module QHask.Conjecture
    ( computeQ
    , computeHm
    , borodinProduct
    , verifyPositivity
    ) where

import qualified Data.Map.Strict as Map

import QHask.QSeries
import QHask.Profile
import QHask.CPP

-- | Compute Q_{n,c}(q) = (q^ell; q^ell)_n * [z^n]((zq;q)_inf * GK_c(z,q))
--
-- The extraction formula:
-- [z^n]((zq;q)_inf * GK_c) = sum_{j=0}^n (-1)^j * q^{j(j+1)/2} / (q)_j * g_{n-j}(q)
computeQ :: Profile -> Int -> Int -> [Integer]
computeQ prof n maxW =
    takeQS (maxW + 1) $ mulQS qellN fn
  where
    d   = profileD prof
    ell = gcd d 3

    -- GK enumeration: need enough room for the extraction
    gk = computeGK prof n maxW

    -- g_m(q) = [z^m] GK_c(z,q) as a QSeries
    gmSeries :: Int -> QSeries
    gmSeries m =
        let coeffMap = extractZCoeff gk m
            maxCoeff = if Map.null coeffMap then 0 else fst (Map.findMax coeffMap)
        in QSeries [ Map.findWithDefault 0 w coeffMap
                   | w <- [0 .. max maxW maxCoeff]
                   ] `addQS` zeroQS  -- ensure infinite via addQS with zeroQS

    -- fn = sum_{j=0}^n (-1)^j * q^{j(j+1)/2} * (1/(q)_j) * g_{n-j}(q)
    fn = foldl addQS zeroQS
        [ scaleQS sign
          $ mulQS (qPower tri)
          $ mulQS (invQJ j)
          $ gmSeries (n - j)
        | j <- [0..n]
        , let sign = if even j then 1 else -1
        , let tri  = j * (j + 1) `div` 2
        ]

    -- 1/(q)_j as a power series
    invQJ :: Int -> QSeries
    invQJ 0 = oneQS
    invQJ j = inverseQS (qFactorial j)

    -- (q^ell; q^ell)_n = prod_{k=1}^n (1 - q^{k*ell})
    qellN = foldl mulQS oneQS
        [ oneMinus (k * ell) | k <- [1..n] ]
    oneMinus e = addQS oneQS (QSeries (replicate e 0 ++ [-1] ++ repeat 0))

-- | Compute h_m(q) = (q)_m * g_m(q), which should have nonneg coefficients.
computeHm :: Profile -> Int -> Int -> [Integer]
computeHm prof m maxW =
    takeQS (maxW + 1) $ mulQS (qFactorial m) (gmSeries m)
  where
    gk = computeGK prof m maxW
    gmSeries mx =
        let coeffMap = extractZCoeff gk mx
        in QSeries [ Map.findWithDefault 0 w coeffMap
                   | w <- [0..]
                   ]

-- | Borodin's product formula for GK_c(1,q).
-- Uses theta functions and q-Pochhammer symbols.
--
-- GK_c(1,q) = (q^{d+r}; q^{d+r})_inf^{r-1}
--           * prod_{pairs} theta(a_ij, d+r)
--           / (q;q)_inf^r
--
-- where r=3 (rank), d = sum(c), and theta(a,m) = (q^a;q^m)_inf * (q^{m-a};q^m)_inf.
borodinProduct :: Profile -> Int -> [Integer]
borodinProduct prof prec =
    takeQS prec result
  where
    d = profileD prof
    (mu0, mu1, _mu2) = profileToMu prof
    r = 3 :: Int
    qdr = d + r

    -- Theta function: theta(a, m) = (q^a; q^m)_inf * (q^{m-a}; q^m)_inf
    theta :: Int -> Int -> QSeries
    theta a m
        | a <= 0 || a >= m = error $ "theta: need 0 < a < m, got a=" ++ show a ++ " m=" ++ show m
        | otherwise = mulQS (qPochhammer a m prec) (qPochhammer (m - a) m prec)

    -- Pairs: (mu_i - mu_j + j - i) for 0 <= i < j <= 2
    -- mu = (mu0, mu1, 0), so:
    --   (i=0,j=1): mu0 - mu1 + 1
    --   (i=0,j=2): mu0 - 0 + 2 = mu0 + 2
    --   (i=1,j=2): mu1 - 0 + 1 = mu1 + 1
    a01 = mu0 - mu1 + 1
    a02 = mu0 + 2
    a12 = mu1 + 1

    -- numerator = (q^qdr; q^qdr)_inf^{r-1} * theta products
    numerator =
        mulQS (powQS (r - 1) (qPochhammer qdr qdr prec))
              (mulQS (theta a01 qdr)
                     (mulQS (theta a02 qdr)
                            (theta a12 qdr)))

    -- denominator = (q;q)_inf^r
    denominator = powQS r (qPochhammer 1 1 prec)

    result = mulQS numerator (inverseQS denominator)

    -- Repeated multiplication
    powQS :: Int -> QSeries -> QSeries
    powQS 0 _ = oneQS
    powQS 1 f = f
    powQS k f = mulQS f (powQS (k-1) f)

-- | Verify that Q_{n,c}(q) has all nonneg coefficients for n = 0..maxN.
verifyPositivity :: Profile -> Int -> Int -> Bool
verifyPositivity prof maxN maxW =
    all (\n -> all (>= 0) (computeQ prof n maxW)) [0..maxN]
