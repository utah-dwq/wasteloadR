#' Calculate allowable effluent temperature discharge from heat loading to a recieving water for wasteload analysis
#' Follow Utah Rivers Model procedure (roughly 2004) with some modifications
#' Function uses seasonal receiving and effluent discharge and temperature for heat load calculation
#' Funtion uses Aquatic Life Classification for stream standard max temperature and max temperature change
#' Function calculates
#' Function returns seasonal effluent temperature limits table
 
#' @param critQ_cfs Upstream seasonal 7Q10 critical flow in CFS
#' @param effluentQ_cfs Seasonal average effluent discharge in CFS
#' @param temp_receiving Receiving water temperature C 
#' @param temp_effluent Effluent discharge water temperature C
#' @param alu

#' @return TBD, table, list, etc

#' @examples
#' heatLoad = function(critQ_cfs=32.0, effluentQ_cfs=7.4326,temp_receiving=13.93, temp_effluent=5.00,aquat_life_use="3B")

#' @export

#need a loop to get seasonal values and run function

heatLoad = function(critQ_cfs=32.0, effluentQ_cfs=7.4326,temp_receiving=13.93, temp_effluent=5.00,aquat_life_use="3B"){
	# Definitions:
	## critQ_cfs Upstream seasonal 7Q10 critical flow in CFS # Yes, need both receiving and effluent Qs for combined
	## effluentQ_cfs Seasonal average effluent discharge in CFS
	## temp_receiving Receiving water temperature C.
	## temp_effluent Effluent discharge water temperature C.
	## aquat_life_use - aquatic life use. One of 3A, 3B, or 3C
	
	
	#calculate heat load (MBTU/d) for receiving and effluent
	heat_load_receiving = critQ_cfs * temp_receiving * 9.7
	heat_load_effluent= = effluentQ_cfs * temp_effluent * 9.7
	
	## Define criteria by use
	#Use AL Standards (T [3A-20C, others 27C] and deltaT [3A-2C, others 4C])
	if(!aquat_life_use %in% c("3A", "3B", "3C")){
		stop("Error. aquat_life_use must be one of '3A', '3B', or '3C'")
	}
	if(aquat_life_use == "3A"){
		temp_criterion=20
		delta_temp=2
	}else{
		temp_criterion=27
		delta_temp=4
	}
		

	#calculate combined Q (upstream & effluent)
	comb_q = critQ_cfs + effluentQ_cfs
	
	#calculate combined temperature ((Qr*Tr)+(Qd*Td))/(Qr+Qd)
	combined_temp = ((critQ_cfs * temp_receiving) + (effluentQ_cfs * temp_effluent)) / (comb_q) 

	#calculate T increase (Tcombined - Treceiving)
	t_increase = combined_temp - temp_receiving

	#check if temperature and temperature increase exceeds standards and note
	temperature_exceed = ifelse(combined_temp > temp_criterion, TRUE, FALSE)
	t_increase_exceed = ifelse(t_increase > delta_temp, TRUE, FALSE)

	#calculate seasonal effluent temperature limit summary
	#	use ((Qu*Tu)+(Qe*Te)) = (Qd*Td) rearranged to Te = ((Qd*Td)-(Qu*Tu))/(Qe)
	temp_effluent_limit=((comb_q * combined_temp) - (critQ_cfs * temp_receiving))/(effluentQ_cfs)
	
	#note and caveat
	# "Note: wasteload analysis may allow unreasonably high allowed temperature and heat loading.'
	# 'Narrative standards, new source performance standards, and BAT also apply, thus possibly'
	# 'reducing the values give in this wasteload analysis.'


	# Gather, return, & print results
	heat_load_receiving
	heat_load_effluent
	
	temp_criterion
	delta_temp
	
	comb_q
	combined_temp
	t_increase
	temperature_exceed
	t_increase_exceed
	temp_effluent_limit
	
}



