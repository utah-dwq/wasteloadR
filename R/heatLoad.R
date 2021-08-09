#' Calculate heat temperature loading for wasteloads
#' 
#' 
 
#' @param q
#' @param t
#' @param alu
#

#' @return TBD, table, list, etc

#' @examples

#' @export
heatLoad = function(q, t, use){
	# Definitions:
	## t ??? Receiving water temperature C? Looks like we might also need effluent t.
	## q - combined effluent + recieving discharge? cfs do we need both recieving water and effluent Qs here again?
	## alu - aquatic life use. One of 3A, 3B, or 3C
	
	#calculate heat load (MBTU/d) for receiving and effluent (Q*T*9.7) - JV what is T here?
	
	
	## Define criteria by use
	#Use AL Standards (T [3A-20C, others 27C] and deltaT [3A-2C, others 4C])
	if(!alu %in% c("3A", "3B", "3C")){stop("Error. alu must be one of '3A', '3B', or '3C')}
	if(alu == "3A"){
		temp_criterion=20
		delta_temp=2
	}else{
		temp_criterion=27
		delta_temp=4
	}
		

	#calculate combined T load ((Qr*Tr)+(Qd*Td))/(Qr+Qd)
	combined_t_load=((Qr*Tr)+(Qd*Td))/(Qr+Qd) 

	#calculate T increase (Tcombined - Treceiving)
	t_increase= combined_t_load - t_receiving

	#check if T+deltaT exceeds standards and note
	t_increase_exceed=ifelse(t_increase>delta_temp, TRUE, FALSE)

	#calculate seasonal effluent temperature limit summary
	#	use ((Qu*Tu)+(Qe*Te)) = (Qd*Td) rearranged to Te = ((Qd*Td)-(Qu*Tu))/(Qe)
	temp_effluent_limit=((Qd*Td)-(Qu*Tu))/(Qe)
	
}



