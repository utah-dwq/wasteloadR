#' Calculate mixing zone statistics for wasteloads
#' Follow STREAMIX I procedure EPA Region 8, Denver, CO, Water Mgmt Div, Dec 1994
#' Function returns table with distance from discharge, travel time and plume width
#' Function returns plume width at 2500 feet distance from discharge and 15 minutes of travel time, expressed as % of stream width

#' @param critQ_cfs Upstream seasonal 7Q10 critical flow in CFS
#' @param effluentQ_cfs Seasonal average effluent discharge in CFS
#' @param shore_dist_ft Discharge distance from shore in feet.
#' @param width_ft Stream channel width in feet
#' @param depth_ft Stream channel depth in feet
#' @param slope Stream channel slope, unitless (typically ft/ft)
#' @param mix_coeff Mixing coefficient estimate. Default is 0.6.
#' @param Q_location Discharge channel location. One of "side" (default) or "center".

#' @return
#' Returns a named list of calculated results.

#' @examples
#' mixing_zone_stats=mixZone(critQ_cfs=736, effluentQ_cfs=2.3, shore_dist_ft=15, width_ft=300, depth_ft=1.8, slope=0.0010, mix_coeff=0.6, Q_location="side")

#' @export
mixZone = function(critQ_cfs, effluentQ_cfs, shore_dist_ft, width_ft, depth_ft, slope, mix_coeff=0.6, Q_location="side"){
	# Testing data
	## Use data in the "Moab_WWTP_WLA_2021.xlsm" file in worksheet "hydraulics". Not the same as worksheet "Stream-Mix", which may be problematic
	#critQ_cfs=736
	#effluentQ_cfs=3.2518
	#shore_dist_ft=15
	#width_ft=300
	#depth_ft=1.8
	#slope=0.0010
	#mix_coeff=0.6
	#Q_location="side"

	# critQ_cfs: upstream seasonal 7Q10 critical Q (with wasteloadR tool pick USGS, DWQ, other sites 20-50 mi upstream using NHD+ reaches)
		# daily USGS for 7Q10, need sites with n>=32, closest to furthest
		# if not use (ie: DWQ) with n>=32
		# if not daily with n>=32, critical flow is 95th% not 80th%
	# effluentQ_cfs: seasonal average effluent discharge or annual (with note) if not seasonal
		# from facility DMR or ICIS like in wasteloadR tool
		# maybe a user picker for time range of data (ie: 5 yr, 10 yr, 20 yr) but typically 10 yr
	# shore_dist_ft: Discharge distance from shore in feet.
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

	if(!Q_location%in%c("side","center")){stop("Q_location must be one of c('side','center')")}

	# Combined Q (upstream & discharge)
	comb_q=critQ_cfs+effluentQ_cfs

	# Channel characteristics and mannings_n
	## defs: n=, A=, R=, S=
	# https://www.omnicalculator.com/physics/hydraulic-radius
		# n: stream channel mannings coefficient (typically 0.030 but variable); mannings_n
		# A: stream channel area (A = width_ft*depth_ft); area_ft2
		# P: stream channel wetted perimeter (trapezoidal, rectangular, other?) (P = 2*depth_ft+width_ft); wet_perim_ft
  		# R: stream channel hydraulic radius (trapezoidal, rectangular, other?) (R = area_ft2/wet_perim_ft); hyd_rad_ft
		# S: (or slope above) the average stream channel slope around the effluent discharge point; slope
		# Q=(1.49/n)*A*R^(2/3)*S^(1/2) # rearrange to calculate n [mannings_n=(1.49/Q)*A*R^(2/3)*S^(1/2)]
	area_ft2 = width_ft*depth_ft
	wet_perim_ft = 2*depth_ft+width_ft
	hyd_rad_ft = area_ft2/wet_perim_ft
	mannings_n=(1.486/comb_q)*area_ft2*(area_ft2/wet_perim_ft)^(2/3)*slope^(1/2)

	# Check reasonable-ness of mannings_n (expected range = 0.018-0.060)
  	# http://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Mannings_n_Tables.htm
  	# https://pubs.usgs.gov/wsp/2339/report.pdf
	if(mannings_n<0.018){warning("Manning's n coefficient <0.018. See http://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Mannings_n_Tables.htm and https://pubs.usgs.gov/wsp/2339/report.pdf for more information.")}
	if(mannings_n>0.060){warning("Manning's n coefficient >0.060. See http://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Mannings_n_Tables.htm and https://pubs.usgs.gov/wsp/2339/report.pdf for more information.")}


	# velocity (ft/sec)
	## defs: Q=combined discharge (comb_q), W=channel width (width_ft), D=channel depth (depth_ft)
	## (V = Q/(W*D))
	velocity_ftsec=comb_q/(width_ft*depth_ft)
	
  	# time at 2500 ft (chronic)
  	time_2500_ft=(2500/velocity_ftsec)/60
  
	# distance at 15 min (acute)
	dist_15min=velocity_ftsec*15*60

	# longitudinal and lateral dispersion coefficients 
	# (Chapra. 1997. Surface Water Quality Modeling. Chap 14.4)
	## defs: G=, D=, S=, D=, C1=
		# G: the gravitational acceleration (32.174 ft.s2)
		G=32.174 #JV note - set to fixed ft.s2 number.
		# D: the average stream channel depth; depth_ft
		# S: the average stream channel slope around the effluent discharge point; S or slope
		# C1: the mixing coefficient (http://webster.ce.gatech.edu/sites/default/files/pubs/asce_chapter.pdf pp 28-30)
  		#     (default value= 0.6 (0.3 - 0.9) +- 50%. Varies with channel irregularity; straight channel uniform flow=0.3,
  		#     curved channel, irreg flow, sidewall interference =1.0, significant meandering can exceed 1.0 to 4.9); mix_coeff
		# U*: shear velocity (ft/s)
		# Q_location: stream effluent discharge location, where side = 1, center = 2
	## shear velocity (ft/s)
	shearVel = sqrt(G*depth_ft*slope) # (Eqn 14.16)

	## Froude number (-)
	froudeNum = velocity_ftsec / sqrt(G*depth_ft) # (pp 246)

	## lateral dispersion coefficient (ft2/s)
	# mix_coeff=0.6 - (Fisher et al., 1979) default value in function currently. Do we want user modified? Or permanent?
  	latDispersionCoeff=shearVel*depth_ft*mix_coeff # (Eqn 14.18)

	if(Q_location == "side"){
		mixLength=0.4*velocity_ftsec*(width_ft^2 / latDispersionCoeff) # (Eqn 14.19)
	}
	if(Q_location == "center"){
		mixLength=0.1*velocity_ftsec*(width_ft^2 / latDispersionCoeff) # (Eqn 14.20)
	}

	## longitudinal dispersion coefficient (ft2/s)
	if (froudeNum <0.5){
		longDispersionCoeff=0.058*comb_q / (slope * width_ft) # (Eqn 14.17; McQuivey and Keefer, 1974)
	}else{
		longDispersionCoeff=0.011*(velocity_ftsec^2 * width_ft^2) / (depth_ft * shearVel) # (Eqn 14.15; Fisher et al., 1979)
	}

	# plume width (CHRONIC at 2500 ft & ACUTE at 15 min)
	## defs: x=, W=, D1, X1=, t1, D
		# x: the distance of effluent discharge from shore in ft (user provided); shore_dist_ft
		# W: the average stream channel width at effluent discharge point; width_ft
		# D1: the lateral dispersion coefficient inf ft2/s; latDispersionCoeff
		# X1: the downstream plume distance, chronic calculated at 2500 ft; xDownChronic_ft
		# t1: the downstream plume travel time, acute calculated at 15 min; tDownAcute_s
		# U: the average downstream velocity (ft/s); velocity_ftsec
	xDownChronic_ft=2500
	plume_widthChronic_ft=(((2*shore_dist_ft/width_ft+1)^2)*2*pi*latDispersionCoeff*(xDownChronic_ft/velocity_ftsec))^0.5 # (Chapra, 1997 Eqn 14.19 rearrange)
	
	tDownAcute_s=15*60 # in seconds
	plume_widthAcute_ft=((2*shore_dist_ft/width_ft+1)^2)*2*pi*latDispersionCoeff*(tDownAcute_s)^0.5 # (Chapra, 1997 Eqn 14.19 rearrange)

	# Chronic theta or plume width as percent of river at 2500 ft and 15 min
		# X1: the downstream plume distance, chronic calculated at 2500 ft; xDownChronic_ft
		# plume width: the plume width across the stream at chronic distance of effluent discharge (2500 ft) in ft; plume_width_ft
		# W: the average stream channel width at effluent discharge point; width_ft
		# Qup: the upstream seasonal 7Q10 critical Q ft3/s; critQ_cfs
		# Qeff: the seasonal average effluent discharge or annual (with note) if not seasonal in ft3/s; effluentQ_cfs
	distPlumePercChronic=(((plume_widthChronic_ft/width_ft*(critQ_cfs+effluentQ_cfs))-effluentQ_cfs)/critQ_cfs)*100 # in percentage
	if(xDownChronic_ft < 0){chronicPlumePercent=0
	}else{
		if(distPlumePercChronic > 0){chronicPlumePercent=distPlumePercChronic
		}else{chronicPlumePercent=0}
	}
	distPlumePercAcute=(((plume_widthAcute_ft/width_ft*(critQ_cfs+effluentQ_cfs))-effluentQ_cfs)/critQ_cfs)*100 # in percentage
	if(tDownAcute_s < 0){acutePlumePercent=0
	}else{
		if(distPlumePercAcute > 0){acutePlumePercent=distPlumePercAcute
		}else{acutePlumePercent=0}
	}

	# chronic flow limit
	chronicQLimit=comb_q*(chronicPlumePercent/100)

	# acute flow limit
	if((distPlumePercAcute/100) < 0.5){acuteQLimit = comb_q * (distPlumePercAcute/100)
	}else{acuteQLimit = chronicQLimit * 0.5}


	# Gather, return, & print results
	result=list(
		chronicQLimit=chronicQLimit,
		acuteQLimit=acuteQLimit,
	
		chronicPlumePercent=chronicPlumePercent,
		acutePlumePercent=acutePlumePercent,
		plume_widthChronic_ft=plume_widthChronic_ft,
		plume_widthAcute_ft=plume_widthAcute_ft,
	    
		mixLength=mixLength,
		longDispersionCoeff=longDispersionCoeff,
		latDispersionCoeff=latDispersionCoeff,
		froudeNum=froudeNum,
		shearVel=shearVel,
		dist_15min=dist_15min,
		time_2500_ft=time_2500_ft,
		velocity_ftsec=velocity_ftsec,
		mannings_n=mannings_n,
		hyd_rad_ft=hyd_rad_ft,
		wet_perim_ft=wet_perim_ft,
		area_ft2=area_ft2
	)
	
	return(result)

}
