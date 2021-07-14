#' Calculate mixing zone statistics for wasteloads
#' Follow STREAMIX I procedure EPA Region 8, Denver, CO, Water Mgmt Div, Dec 1994
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
		# Q=(1.49/n)*A*R^(2/3)*S^(1/2) # rearrange to calculate n [mannings_n=(1.49/Q)*A*R^(2/3)*S^(1/2)]
	area_ft2 = width_ft*depth_ft
	hyd_rad_ft = 2*depth_ft+width_ft
	mannings_n=(1.486/comb_q)*area_ft2*hyd_rad_ft^(2/3)*slope^(1/2)]
	
	# Check reasonable-ness of mannings_n (expected range = 0.018-0.060)
		# maybe reference table of mannings n values for user (http://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Mannings_n_Tables.htm or
		# https://pubs.usgs.gov/wsp/2339/report.pdf)
	if(mannings_n<0.018){warning("Manning's n coefficient <0.018")}
	if(mannings_n>0.060){warning("Manning's n coefficient >0.060")}
	
	
	# velocity (ft/sec)
	## defs: Q=combined discharge (comb_q), W=channel width (width_ft), D=channel depth (depth_ft)
	## (V = Q/(W*D))
	velocity_ftsec=comb_q/(width_ft*depth_ft)
	
	# distance at 15 min
	dist_15min=velocity_ftsec*15*60
	
	# longitudinal and lateral dispersion coefficients (Chapra. 1997. Surface Water Quality Modeling. Chap 14.4)
	## defs: G=, D=, S=, D=, C1=
		# G: the gravitational acceleration (9.81 m/s2 or 32.174 ft.s2)
		# D: the average stream channel depth; depth_ft
		# S: the average stream channel slope around the effluent discharge point; S or slope
		# C1: the mixing coefficient (default value= 0.6 (0.3 - 0.9). Varies with channel irregularity; straight channel uniform flow=0.3, 
		#     curved channel, irreg flow, sidewall interference =1.0, significant meandering can exceed 1.0); mix_coeff
		# U*: shear velocity (ft/s)
		# Q_location: stream effluent discharge location, where side = 1, center = 2
	## shear velocity (ft/s)
	shearVel = Sqrt(G*depth_ft*slope)
	
	## Froude number (-)
	froudeNum = velocity_ftsec / Sqrt(G*depth_ft)
	
	## lateral dispersion coefficient (ft2/s)
	mix_coeff=0.6
	latDispersionCoeff=shearVel*depth_ft*mix_coeff
	
	if Q_location = 1
		mixLength=0.4*velocity_ftsec*(width_ft^2 / latDispersionCoeff)
	if Q_location = 2
		mixLength=0.1*velocity_ftsec*(width_ft^2 / latDispersionCoeff)
	
	## longitudinal dispersion coefficient (ft2/s)
	if froudeNum <0.5, then
		longDispersionCoeff=0.058*comb_q / (slope * width_ft) # (McQuivey and Keefer, 1974)
	else
		longDispersionCoeff=0.011*((velocity_ftsec^2 * width_ft^2) / (depth_ft * shearVel) # (Fisher et al., 1979)
	
	# plume width (ft & %)
	## return plume width at multiple distances - user specifies maximum distance and distance interval
		## Jake, we only need the 2500 ft distance and 15 min period. No need to do multiple distances
	## defs: x=, W=, D1, X2=, D
		# x: the distance of effluent discharge from shore in ft (user provided); xShore_ft
		# W: the average stream channel width at effluent discharge point; width_ft
		# D1: the lateral dispersion coefficient inf ft2/s; latDispersionCoeff
		# X2: the downstream plume distance, chronic calculated at 2500 ft; xDownstream_ft
		# U: the average downstream velocity (ft/s); velocity_ftsec
	target_distances=seq(dist_int_ft, max_dist_ft, by=dist_int_ft) # if want sequence then the following. Maybe start at -3 intervals
		# dist_int_ft: the downstream distance interval for the sequence in ft; 5
		# max_dist_ft: the maximum downstream distance for the sequence in ft; 5280
	xDownstream_ft=2500
	plume_width_ft=(((2*xShore_ft/width_ft+1)^2)*PI()*latDispersionCoeff*(xDownstream_ft/velocity_ftsec))^0.5 # can apply equation across all target distances.
	
	# Chronic theta or plume width as percent of river at 2500 ft
	## defs: x=, W=, D1, X2=, D
		# X2: the downstream plume distance, chronic calculated at 2500 ft; xDownstream_ft
		# plume width: the plume width across the stream at chronic distance of effluent discharge (2500 ft) in ft; plume_width_ft
		# W: the average stream channel width at effluent discharge point; width_ft
		# Qup: the upstream seasonal 7Q10 critical Q ft3/s; streamQcfs
		# Qeff: the seasonal average effluent discharge or annual (with note) if not seasonal in ft3/s; effluentQcfs
	distPlumePercent=(((plume_width_ft/width_ft*(streamQcfs+effluentQcfs))-effluentQcfs)/streamQcfs)*100 # in percentage
	if xDownstream_ft < 0, then chronicPlumePercent=0
	else
		if distPlumePercent > 0, then chronicPlumePercent=distPlumePercent
		else chronicPlumePercent=0

	# chronic flow limit
	chronicQLimit=comb_q*chronicPlumePercent
	
	# acute flow limit
	## if (plume width % of river at 15 min) < 0.5, then Qcombined * (plume width % of river at 15 min)
	## else (chronic flow * 0.5)
	
	
	# Gather, return, & print results
	
	
	
}

