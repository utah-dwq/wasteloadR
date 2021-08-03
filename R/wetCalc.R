#' Whole effluent toxicity (WET) test calculations for Utah River's model
#' 
#' 
 
#' @param critQ_cfs Upstream seasonal critical flow (typically 7Q10) in CFS
#' @param effluentQ_cfs Seasonal average effluent discharge in CFS
#' @param width_ft Stream channel width in feet (also used in mixZone())
#' @param plume_width_15min_ft Plume width at 15 min or 50%: determined in mixing zone function (mixZone() output)
#' @param plume_width_2500ft_ft Plume width at 2500 ft: determined in mixing zone function (mixZone() output)
#

#' @return TBD

#' @examples

#' @export
wetCalc = function(critQ_cfs, effluentQ_cfs, width_ft, plume_width_15min_ft, plume_width_2500ft_ft){
	
	# Arg definitions and notes from pseudocode
	## critQ_cfs: River Q (7Q10): summer critical flow value in background Q and WQ function
	## effluentQ_cfs: WWTP Q: summer projected or design flow from WLA effluent function
	## width_ft: River width: determined earlier with map (also used in mixZone())
	## plume_width_15min_ft: Plume width at 15 min or 50%: determined in mixing zone function (mixZone() output)
	## plume_width_2500ft_ft: Plume width at 2500 ft: determined in mixing zone function (mixZone() output)
	
	# Process
	## calculate acute dilution Q: (Plume width at 15 min or 50%)/W*Qcritical
	## calculate chronic dilution Q: (Plume width at 2500 ft)/W*Qcritical
	## determine if totally mixed or incompletely mixed
	#	No - Qeff (cfs) * 2 <= Qup
	#		ratio of mix to design: (Qcritical/2 + Qeff)/Qeff
	#			if > 3.33 calc, else end of pipe (EOP)
	#		acute % effluent: 
	#			if ratio > 3.33 AND not totally mixed, then EOP
	#			else Qeff/(0.3* (Qeff + Qcrit/2))
	#		chronic % effluent: 
	#			Qeff/(Qeff + Qcrit)
	#	Yes - Qeff (cfs) * 2 > Qup
	#		ratio of mix to design: (Qeff + [acute dilution Q])/Qeff
	#		acute % effluent: 
	#			if ratio > 3.33 Qeff/(0.3 * [acute dilution Q])
	#			else EOP
	#		chronic % effluent: 
	#			Qeff/(Qeff + [chronic dilution Q])

	
	
	
}



