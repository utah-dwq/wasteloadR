#' Calculate mixing zone statistics for wasteloads
#' Function returns table with distance from discharge, travel time and plume width
#' Function returns plume width at 2500 feet distance from discharge and 15 minutes of travel time, expressed as % of stream width
 
#' @param streamQcfs Upstream discharge in CFS
#' @param effluentQcfs Effluent discharge in CFS
#' @param dist_ft Discharge distance from shore in feet.
#' @param width_ft Stream channel width in feet
#' @param depth_ft Stream channel depth in feet
#' @param slope Stream channel slope, unitless
#' @param mixing_coef Mixing coefficient estimate. Default is 0.6.
#' @param max_dist_ft Maximum distance to calculate plume width in feet. Default 2500.
#' @param dist_int_ft Interval distance for plume width calculations in feet. Deafult 500. Plume widths are calculated from dist_int_ft to max_dist_ft with an interval of dist_int_ft.
#

#' @return TBD, table, list, etc

#' @import dplyr
#' @importFrom lubridate year

#' @examples

#' @export
mixZone = function(streamQcfs, effluentQcfs, dist_ft, width_ft, depth_ft, slope, mixing_coef=0.6, max_dist_ft=2500, dist_int_ft=500){
		# streamQcfs: upstream seasonal 7Q10 critical Q (with wasteloadR tool pick USGS, DWQ, other sites 20-50 mi upstream using NHD+ reaches)
			# daily USGS for 7Q10, need sites with n>=32, closest to furthest
			# if not use (ie: DWQ) with n>=32
			# if not daily with n>=32, critical flow is 95th% not 80th%
		# effluentQcfs: seasonal average effluent discharge or annual (with note) if not seasonal
			# from facility DMR or ICIS like in wasteloadR tool
			# maybe a user picker for time range of data (ie: 5 yr, 10 yr, 20 yr) but typically 10 yr
		# dist_ft: the xlsx code used an iterative 100ft distance to 5000ft and then a vlookup to pull data. We can just code the distance required as below.
		# width_ft: the stream channel width at effluent discharge point
			# can be user supplied, from NHD+ reach info, USGS stream characteriz, or other
		# depth_ft: the average stream channel depth at effluent discharge point
			# can be user supplied, from NHD+ reach info, USGS stream characteriz, DEM adjacent slope estimate, or other
		# slope: the average stream channel slope around the effluent discharge point (ft/ft)
			# can be user supplied, from NHD+ reach info
			# with NHD+, get elevations 1, 2, and 3 mi up and downstream
			# then calc ave river elev drop (ft/mi) across the distances
			# if elev drop < 6.2 ft/mi then natural, otherwise channelized
			# calculate stream sinuosity from (reach distance : linear distance)
	
	# Combined Q (upstream & discharge)
	comb_q=streamQcfs+effluentQcfs
	
	# mannings_n #pull out to separate function? Or is automatic calculation here preferred?
	## defs: n=, A=, R=, S=
		# n: stream channel mannings coefficient (typically 0.030 but variable); mannings_n
		# A: stream channel area (A = width_ft*depth_ft); area_ft2
		# R: stream channel hydraulic radius (trapezoidal, rectangular, other?) (R = 2*depth_ft+width_ft); hyd_rad_ft
		# S: (or slope above) the average stream channel slope around the effluent discharge point; slope
	#Q=(1.49/n)*A*R^(2/3)*S^(1/2) # rearrange to calculate n [mannings_n=(1.49/Q)*A*R^(2/3)*S^(1/2)]
	A = width_ft*depth_ft
	R = 2*depth_ft+width_ft
	mannings_n=(1.49/comb_q)*A*R^(2/3)*slope^(1/2)]
	
	# Check reasonable-ness of mannings_n (expected range = 0.018-0.060)
	if(mannings_n<0.018){warning("Manning's n coefficient <0.018")}
	if(mannings_n>0.060){warning("Manning's n coefficient >0.060")}
	
	
	# velocity (ft/sec)
	## defs: Q=combined discharge (comb_q), W=channel width (width_ft), D=channel depth (depth_ft)
	## (V = Q/(W*D))
	velocity_ftsec=comb_q/(width_ft*depth_ft)
	
	# distance at 15 min
	dist_15min=velocity_ftsec*15*60
	
	# dispersion coefficient (need to check on this)
	## defs: G=, D=, S=, D=, C1=
		# G: the gravitational acceleration 
		# D: the stream channel width; width_ft
		# S: the average stream channel slope around the effluent discharge point; S or slope
		# C1: the mixing coefficient (typically 0.6 but should vary); mix_coeff
	## D1 = Sqrt(G*D*S)*D*C1
	dispersion=Sqrt(G*D*S)*D*C1
	
	# plume width (ft & %)
	## return plume width at multiple distances - user specifies maximum distance and distance interval
	## defs: x=, W=, D1=dispersion coefficient, distance=, D=
	target_distances=seq(dist_int_ft, max_dist_ft, by=dist_int_ft)
	plume_width_ft=(((2*x/W+1)^2)*2*PI()*D1*distance/D)^0.5 #apply equation across all target distances.
	
	# theta 
	## if(distance < 0, then 0)
	## if(((plume width/W*(Qup+Qeff))-Qeff)/Qup > 0,then this calculation)
	## else 0

	# chronic flow
	## Q combined * (plume width % of river at 2500 ft)
	
	# acute flow
	## if (plume width % of river at 15 min) < 0.5, then Qcombined * (plume width % of river at 15 min)
	## else (chronic flow * 0.5)
	
	
	# Gather, return, & print results
	
	
	
}

