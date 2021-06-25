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
	# need variable definitions - reformat equations to use function parameter names (or vice-versa)
	
	# Combined Q (upstream & discharge)
	Q=streamQcfs+effluentQcfs
	
	# mannings_n
	## defs: n=, A=, R=, S=
	mannings_n=(1.49/n)*A*R^(2/3)*S^(1/2)
	
	# Check reasonable-ness of mannings_n (expected range = x-y)
	
	# velocity
	## defs: Q=, W=, D
	(V = Q/(W*D))
	
	# distance at 15 min
	dist_15min=V*t
	
	# dispersion coefficient
	D1 = Sqrt(G*D*S)*D*C1
	
	# plume width (ft & %)
	## return plume width at multiple distances - user specifies maximum distance and distance interval
	target_distances=seq(dist_int_ft, max_dist_ft, by=dist_int_ft)
	plume_width_ft=(((2*x/W+1)^2)*2*PI()*D1*distance/D)^0.5 #apply equation across all target distances.
	
	# theta 
	if(distance < 0, then 0)
	if(((plume width/W*(Qup+Qeff))-Qeff)/Qup > 0,then this calculation)
	else 0

	# chronic flow
	Q combined * (plume width % of river at 2500 ft)
	
	# acute flow
	if (plume width % of river at 15 min) < 0.5, then Qcombined * (plume width % of river at 15 min)
	else (chronic flow * 0.5)
	
	
	# Gather, return, & print results
	
	
	
}

