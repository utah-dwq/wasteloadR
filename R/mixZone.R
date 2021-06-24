#' Calculate mixing zone statistics for wasteloads
 
#' @param streamQcfs
#' @param effluentQcfs
#' @param dist_ft
#' @param width_ft
#' @param depth_ft
#' @param slope_ftft
#' @param mixing_coef
#

#' @return Either a numeric value of nQy or a dataframe of minimum annual n-day average discharge values.

#' @import dplyr
#' @importFrom lubridate year
#' @importFrom roll roll_mean

#' @examples

#' @export
mixZone = function(streamQcfs, effluentQcfs, dist_ft, width_ft, depth_ft, slope_ftft, mixing_coef=0.6){
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
	
	# plume width
	## specify target distance as a function input? Or return widths at multiple distances? Or both (i.e. supply a vector of target lengths)?
	plume_width_ft=(((2*x/W+1)^2)*2*PI()*D1*distance/D)^0.5
	
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

