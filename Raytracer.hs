-- :l codes/haskell/Raytracer.hs

module Raytracer
    where

import Geom
import Scene


-- FIX make one sided plane
plane_test       :: [Double] -> [Double] -> [(Double, Plane)]
plane_test v0 vd =
    filter (\x -> (fst x) > sdist)
	   (zip
	    (map (\x -> let id = vdot (normal x) vd
		        in if (id > 1e-9) || (id < (-1e-9))
		              then ((-1) * (vdot (normal x) v0) + (d x)) / id
		              else (-1))
	     planes)
	    planes)

is_hit_plane       :: [Double] -> [Double] -> (HitCo, Material)
is_hit_plane v0 vd =
    let minis = plane_test v0 vd
    in if (length minis) == 0
  	   then (nullhc, nullmat)
  	   else let mobj = minimum minis
 	        in let vpi = vadd v0 (vmults (fst mobj) vd)
		       vsn = normal (snd mobj)
		   in (Hc vpi vsn (fst mobj) (plref (snd mobj)), (matp (snd mobj)))



sphere_test2       :: [Double] -> [Double] -> [(Double, Sphere)]
sphere_test2 v0 vd =
    filter
    (\x -> (fst x) > 0)
    (zip
     (map (\x -> (let siB = 2 * (vdot vd (vsub v0 (center x)))
  	              siC = (foldl (+) (0) (map (**2) (vsub v0 (center x)))) - (rad x)**2
  	              di  = discrs siB siC
		  in if di > 0
		        then let sl = filter (\x -> x > sdist) (solv2s siB siC di)
		             in if (length sl) == 0
		                   then (-1)
		                   else minimum sl
		        else (-1)))
      spheres)
     spheres)


-- now calc normals

is_hit_sphere       :: [Double] -> [Double] -> (Int,Double) -> (HitCo, Material)
is_hit_sphere v0 vd inside =
    let minis = sphere_test2 v0 vd
    in if (length minis) == 0
  	   then (nullhc, nullmat)
  	   else let mobj = minimum minis
 	        in let vpi = vadd v0 (vmults (fst mobj) vd)
		       vsn = map (\x -> x/(rad (snd mobj))) (vsub vpi (center (snd mobj)))
		   in if (fst inside) == 0
		         then (Hc vpi vsn (fst mobj) (spref (snd mobj)), (mat (snd mobj)))
		         else (Hc vpi (vmults (-1) vsn) (fst mobj) (spref (snd mobj)), (mat (snd mobj)))


--is_hit_any       :: [Double] -> [Double] -> [Double]
is_hit_any v0 vd =
    let minis = (map (\x -> mat (snd x)) (sphere_test2 v0 vd))
		++ (map (\x -> matp (snd x)) (plane_test v0 vd))
    in if (length minis) == 0
           then [1,1,1]
	   else foldl (\x y -> vmult x (vmults (t y) (color y))) [1,1,1] minis


best_hit       :: [Double] -> [Double] -> (Int,Double) -> (HitCo, Material)
best_hit v0 vd inside =
    let hits = filter (\x -> (dist (fst x)) > 0) [(is_hit_sphere v0 vd inside), (is_hit_plane v0 vd)]
    in if (length hits) == 0
           then (nullhc, nullmat)
	   else minimumhc hits
--	   else head hits

--rt_trace                    :: [Double] -> [Double] -> Int -> Boolean -> [Double]
rt_trace v0 vd depth inside =
    let hit = best_hit v0 vd inside
    in if (dist (fst hit)) <= 0
           then bg_color
	   else rt_shade v0 vd (fst hit) (snd hit) depth inside


-- SHADING

-- Basic ambient calculation, slightly modified so that there is
-- no global component

shade_amb mat =
    vmult (am mat) (color mat)

-- Diffuse and specular components according to Whitted's model,
-- with attenuation added as in CGPaP 2ed and the model extended
-- with Warn's lights and it takes into account being shadowed by
-- semitransparent objects

shade_dfspec v0 vd hco mat =
    foldl (\x y -> let vslight = vsub (coord y) (vi hco)
	           in if (vdot vslight (vsn hco)) > 0
	                 then let ldist = vlen vslight
	                          nvslight = vnorm vslight
	                          blocking = is_hit_any (vi hco) nvslight
	                      in if (sum blocking) > 0
	                            then let lc = vmult (vmults (abs ((vdot nvslight (nl y))**(warnc y))) (lcolor y)) blocking
	                                     fatt = min 1 (75.0/(0.05 + 0.05*ldist + 0.1*ldist**2))
	                                     diffuse = vmults ((df mat) * (abs (vdot nvslight (vsn hco)))) (color mat)
	                                     ophong = abs (vdot (vrefl (vmults (-1) nvslight) (vsn hco)) (vmults (-1) vd))
	                                     ospec = vmults ((spec mat) * ophong**(phong mat)) lc
	                                 in vadd x (vmult (vmults fatt lc) (vadd diffuse ospec))
	                            else vadd x [0,0,0]
	                 else vadd x [0,0,0])
           [0,0,0]
	   lights



-- Reflected component

shade_refl v0 vd hco mat depth =
    if ((ref mat) > 0) && (depth < maxdepth)
       then vmults (ref mat) (rt_trace (vi hco) (vrefl vd (vsn hco)) (depth + 1) (0,1))
       else [0,0,0]


-- Calculates the relative ior based on whether we're inside an object
-- or not

get_ior hco inside curr_ior =
    if (fst inside) == 0
       then 1.0/curr_ior
       else if (fst inside) == (insref hco)
	       then curr_ior
	       else (snd inside)/curr_ior


-- The transparent component

shade_transp v0 vd hco mat depth inside =
    if ((t mat) > 0) && (depth < maxdepth)
       then if ((ior mat) == 1) && ((fst inside) == 0)
	       then vmults (t mat) (rt_trace (vi hco) vd (depth + 1) (0,1))
	       else let nr = get_ior hco inside (ior mat)
			sq = refrtSq (vmults (-1) vd) (vsn hco) nr
			nin = if (fst inside) == (insref hco) then (0,1) else (insref hco, ior mat)
		    in if sq < 0 --total reflection
		       then rt_trace (vi hco) (vrefl vd (vsn hco)) (depth + 1) nin
		       else vmults (t mat) (rt_trace (vi hco) (vrefr vd (vsn hco) nr sq) (depth + 1) nin)
       else [0,0,0]


rt_shade v0 vd hco mat depth inside =
    let amb_color = shade_amb mat
	ds_color = vadd amb_color (shade_dfspec v0 vd hco mat)
	refl_color = vadd ds_color (shade_refl v0 vd hco mat depth)
	transp_color = vadd refl_color (shade_transp v0 vd hco mat depth inside)
    in normalize_colors transp_color


normalize_colors cv =
    map (\x -> if x > 1.0 then 1.0 else x) cv

--render_pixel     :: Int -> Int -> [Double]
render_pixel x y =
    let vp = [xcoord x, ycoord y, 0.0]
	sdist = vlen vrp
	vd = vnorm (vsub vp vrp)
	v0 = vrp
    in rt_trace v0 vd 1 (0,1)
