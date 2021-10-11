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
	### JV to CS: mixZone puts out both acute & chronic plume widths which ones are these? Or do you do this twice?
	## plume_width_15min_ft: Plume width at 15 min or 50%: determined in mixing zone function (mixZone() output). 
	## plume_width_2500ft_ft: Plume width at 2500 ft: determined in mixing zone function (mixZone() output)
	
	# Process
	## calculate acute dilution Q: (Plume width at 15 min or 50%)/W*Qcritical #JV: Is W here river width?
	acute_dilution_Q=plume_width_15min_ft/width_ft*critQ_cfs
	
	## calculate chronic dilution Q: (Plume width at 2500 ft)/W*Qcritical #JV: Is W here river width?
	chronic_dilution_Q=plume_width_2500ft_ft/width_ft*critQ_cfs	
	
	## determine if totally mixed or incompletely mixed
	fully_mixed=ifelse(effluentQ_cfs * 2 <= critQ_cfs, TRUE, FALSE) # Generating separate fully_mixed object so we can print/return it for user review, used in if() statement below	
	
	#	Yes - Qeff (cfs) * 2 > Qup # JV to CS: Double my check fully_mixed ifelse above. I interpreted Qeff (cfs) as effluentQ_cfs & Qup as critQ_cfs
	#					CS to JV: I think above is >, not <=. The above implies mixed if less than 50% I think
	#		ratio of mix to design: (Qeff + [acute dilution Q])/Qeff
	#		acute % effluent: 
	#			if ratio > 3.33 Qeff/(0.3 * [acute dilution Q])
	#			else EOP
	#		chronic % effluent: 
	#			Qeff/(Qeff + [chronic dilution Q])
	#	No - Qeff (cfs) * 2 <= Qup  
	#		ratio of mix to design: (Qcritical/2 + Qeff)/Qeff
	#			if > 3.33 calc, else end of pipe (EOP)
	#		acute % effluent: 
	#			if ratio > 3.33 AND not totally mixed, then EOP #JV note to CS: seems to conflict w/ above 'if > 3.33 calc'
	#			else Qeff/(0.3* (Qeff + Qcrit/2))
	#		chronic % effluent: 
	#			Qeff/(Qeff + Qcrit)

	if(fully_mixed){ #Fully mixed
		mix_design_ratio=(effluentQ_cfs + acute_dilution_Q)/effluentQ_cfs
		if(mix_design_ratio > 3.33){
			acute_pct_effluent=effluentQ_cfs/(0.3 * acute_dilution_Q) #If ratio >3.33 & fully_mixed==TRUE
		}else{
			acute_pct_effluent="End of pipe" #If ratio <=3.33 & fully_mixed
		}
		chronic_pct_effluent=effluentQ_cfs/(effluentQ_cfs + chronic_dilution_Q) #fully_mixed==TRUE
	}else{ #not fully mixed
		mix_design_ratio=(critQ_cfs/2 + effluentQ_cfs) / effluentQ_cfs
		if(mix_design_ratio > 3.33){
			acute_pct_effluent=effluentQ_cfs/(0.3 * (effluentQ_cfs + critQ_cfs/2)) #If ratio >3.33 & fully_mixed==FALSE
		}else{
			acute_pct_effluent="End of pipe" #If ratio <=3.33 & fully_mixed==FALSE
		}	
	}
	
	# Compile results to list, print, and return
	result=list(acute_dilution_Q=acute_dilution_Q, chronic_dilution_Q=chronic_dilution_Q, 
				fully_mixed=fully_mixed, mix_design_ratio=mix_design_ratio, 
				acute_pct_effluent=acute_pct_effluent, chronic_pct_effluent=chronic_pct_effluent)
	print(result)
	return(result)
}



