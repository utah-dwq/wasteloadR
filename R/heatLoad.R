#' Calculate heat temperature loading for wasteloads
#' 
#' 
 
#' @param q
#' @param t
#

#' @return TBD, table, list, etc

#' @examples

#' @export
heatLoad = function(q, t){
	#calculate heat load (MBTU/d) for receiving and effluent (Q*T*9.7)
	#Use AL Standards (T [3A-20C, others 27C] and deltaT [3A-2C, others 4C])
	#calculate combined T load ((Qr*Tr)+(Qd*Td))/(Qr+Qd)
	#calculate T increase (Tcombined - Treceiving)
	#check if T+deltaT exceeds standards and note
	#calculate seasonal effluent temperature limit summary
	#	use ((Qu*Tu)+(Qe*Te)) = (Qd*Td) rearranged to Te = ((Qd*Td)-(Qu*Tu))/(Qe)
}



