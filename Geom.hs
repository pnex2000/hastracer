-- :l codes/haskell/Geom.hs

module Geom
    where

-- basic vector operations

-- vmults       :: Num -> [Num] -> [Num]
vmults i v   = map (i*) v
vadd v1 v2   = zipWith (+) v1 v2
vsub v1 v2   = zipWith (-) v1 v2
vmult v1 v2  = zipWith (*) v1 v2

vlen v       = sqrt (foldl (+) 0 (map (**2) v))
vnorm v      = map (/(vlen v)) v
vdot v1 v2   = foldl (+) 0 (zipWith (*) v1 v2)
vangle v1 v2 = acos ((vdot v1 v2) / ((vlen v1) * (vlen v2)))

-- basic 2nd level polynomial solving

discr         :: Double -> Double -> Double -> Double
discr a b c   = b**2 - 4*a*c
solv2 a b c d =
    let rside = sqrt d
	uside = 2*a
    in [(-b + rside)/uside, (-b - rside)/uside]

--solv2g        :: Double -> Double -> Double -> [Double]
--solv2g a b c  = solv2(a, b, c, discr a,b,c)

-- When a=1, which is useful in ray-tracing
discrs        :: Double -> Double -> Double
discrs b c    = b**2 - 4*c
solv2s b c d  =
    let rside = sqrt d
    in [(-b + rside)/2, (-b - rside)/2]

-- Snell's law on reflection applied
-- Rout = 2Ncos(q) - Rin = 2N(N * Rin) - Rin
-- where N = vsn and Rin = -vd
vrefl vd vsn =
    let vin = vmults (-1) vd
    in vsub (vmults (2*(vdot vsn vin)) vsn) vin

-- imaginary if total reflection occurs
refrtSq vi vsn nr = 1 - nr**2 * (1 - (vdot vsn vi)**2)


-- T = ( nr(N*I) - sqrt(1-nr**2(1-(N*I)**2)) )N - nrI
-- where I = -vd, nr = ni/nt
vrefr vd vsn nr sq =
    let vin = vmults (-1) vd
    in vsub (vmults (nr * (vdot vsn vin) - (sqrt sq)) vsn) (vmults nr vin)
