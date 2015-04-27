-- :l codes/haskell/Scene.hs

module Scene
    where

import Geom

-- Camera

maxdepth = 9
vrp      = [0.0, 0.0, (-20.0)]

xleft    = (-12)
xright   = 12
ytop     = 9
ybottom  = (-9)

swidth   = 640
sheight  = 480

dx       = (xright - xleft) / swidth
dy       = (ytop - ybottom) / sheight

xcoord px = xleft + (0.5 + px) * dx
ycoord py = ytop - (0.5 + py) * dy

-- minimum distance CHECK
sdist = 0.0001


-- datatype defs

data Material = Mat {color, am :: [Double], df, ref, t, ior, spec, phong :: Double}
	    deriving (Eq)

instance Show Material where
  show s = show (color s)
-- This really doesn't matter since we can't ever compare materials,
-- but Haskell's type system demands it
instance Ord Material where
  x < y = (df x) < (df y)
  x > y = (df x) > (df y)


getMaterial   :: Material -> [Double]
getMaterial c = color c

data Sphere = Sp {spref :: Int, center :: [Double], rad :: Double, mat :: Material}
	    deriving (Eq)

instance Show Sphere where
  show s = show (center s)
instance Ord Sphere where
  x < y = (rad x) < (rad y)


getSColor   :: Sphere -> [Double]
getSColor c = color (mat c)


data Plane = Pl {plref :: Int, normal :: [Double], d :: Double, matp :: Material}
	    deriving (Eq)

instance Show Plane where
  show p = show (normal p)
instance Ord Plane where
  x < y = (d x) < (d y)


data HitCo = Hc {vi, vsn :: [Double], dist :: Double, insref :: Int}
	   deriving (Eq)

instance Show HitCo where
  show h = show [(vi h),(vsn h),[(dist h)]]
instance Ord HitCo where
  x < y = (dist x) < (dist y)
  x > y = (dist x) > (dist y)

minimumhc []       = error "Prelude.minimum: empty list"
minimumhc [x]      = x
minimumhc (x:y:xs) = if  (fst x) < (fst y) then minimum (x:xs)  
                     else minimum (y:xs)


nullmat = Mat [0,0,0] [0,0,0] 0 0 0 0 0 0
nullhc = Hc [0,0,0] [0,0,0] 0 0

data Light = Li {coord, lcolor, nl :: [Double], warnc :: Double}
	    deriving (Eq)

-- Lights

--lights = [[(-10.0),15.0,0.0]]
lights = [Li [-20,45,0] (vmults 3 [1,1,1]) (vnorm [20,-45,0]) 16]

bg_color = [0.2,0.7,1.0]


--- objects

--spheres = []
spheres = [Sp 100 [0, 0, 12] 5.0 (Mat [1.0,0,0] [0.2,0,0] 0.6 0.2 0 1 0.8 40)]
	  ++ [Sp 101 [-9,6,8] 4.0 (Mat [0,1.0,0] [0,0.1,0] 0.3 0.4 0 1 0.3 10)]
	  ++ [Sp 102 [-4,-3,0] 4.0 (Mat [0,0.3,1.0] [0.2,0.2,0.2] 0.8 0 0 1 0.15 5)]
	  ++ [Sp 103 [3,4,3] 2.9 (Mat [0.4,0.8,1.0] [0,0,0] 0.2 0.1 0.8 1.33 0.95 60)]
	  ++ [Sp 104 [-1,2,3] 1.0 (Mat [1,1,1] [0,0,0] 0.05 0 1 1.7 1 100)]

--planes = []
planes = [Pl 20 [0, 1, 0] (-6) (Mat [1.0,0.5,0] [0.1,0.2,0.2] 0.6 0 0 1 0.2 7)]

