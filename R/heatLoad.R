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
#' @param aquat_life_use - aquatic life use. One of 3A, 3B, or 3C

#' @return TBD, table, list, etc

#' @examples
#' result = heatLoad(critQ_cfs=32.0, effluentQ_cfs=7.4326,temp_receiving=13.93, temp_effluent=5.00, aquat_life_use="3B")

#' @return
#' Returns a named list of calculated results.

#' @export
heatLoad = function(critQ_cfs, effluentQ_cfs, temp_receiving, temp_effluent,aquat_life_use){
  # Definitions:
  ## critQ_cfs Upstream seasonal 7Q10 critical flow in CFS # Yes, need both receiving and effluent Qs for combined
  ## effluentQ_cfs Seasonal average effluent discharge in CFS
  ## temp_receiving Receiving water temperature C.
  ## temp_effluent Effluent discharge water temperature C.
  ## aquat_life_use - aquatic life use. One of 3A, 3B, or 3C
  
  #need a loop to get seasonal values and run function (comment from Chris)
    # JV note: A few options for this:
	# 1. apply() or ddply() to a dataframe of input vars
	# 2. Much of the function already works w/ vector inputs (e.g. critQ_cfs=c(32.0, 28, 22, 25)), so we could probably allow vectors as arguments. We'd need to loop the if() statement parts. 
	#    We'd probably want to name the seasons as an arg and formulate the results.
	# 3. Simplest approach is just to call heatLoad() once for each season and give it appropriate args for each season. We can make core results stackable.
	# I think we're OK for now viewing this function as a single season function that can be applied to multiple seasons in the future

  #calculate initial heat loads (MBTU/d) for receiving and effluent
  # the 9.7 multiplier is suspect, need reference
  # I think the equation is MBTU = ft3/s*(448.83 GPM/ft3/s)*(8.33 lb/gal)*(60 min/hr)*(1 MBTU/1000000 BTU)*(Teff-Tup)
  heat_load_receiving_init = critQ_cfs * temp_receiving * 9.7
  heat_load_effluent_init= effluentQ_cfs * temp_effluent * 9.7
  
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
  
  #calculate initial combined temperature ((Qr*Tr)+(Qd*Td))/(Qr+Qd)
  combined_temp_init = ((critQ_cfs * temp_receiving) + (effluentQ_cfs * temp_effluent)) / (comb_q) 
  
  #calculate initial temperature increase (Tcombined - Treceiving)
  t_increase_init = combined_temp_init - temp_receiving
  
  #solve effluent temperature at temperature increase standard
  # rearrange (Qup*Tup) + (Qeff*Teff) = (Qdn*Tdn) for Tdn
  # then use Tincrease =Tdn-Tup at Tincrease standard where Tdn is previous eqn
  # solve for Teff = (((Tincrease+Tup)*Qdn)-(Qup*Tup))/Qeff
  temp_effluent_increase_limit=(((delta_temp+temp_receiving)*comb_q)-(critQ_cfs * temp_receiving))/(effluentQ_cfs)
  
  # may need eqn here
  # solved Teff limit at max temp increase but if combined_temp > temp_criterion, drop Teff limit even more, solve this
  temp_effluent_temp_limit = ((comb_q * temp_criterion) - (critQ_cfs * temp_receiving)) / (effluentQ_cfs)
  
  # then if statement to pick the lower of the limits
  if(temp_effluent_increase_limit < temp_effluent_temp_limit){
    temp_effluent_limit=temp_effluent_increase_limit
  }else{
    temp_effluent_limit=temp_effluent_temp_limit
  }
  
  #calculate final heat loads (MBTU/d) for receiving and effluent
  # the 9.7 multiplier is suspect, need reference
  # I think the equation is MBTU = ft3/s*(448.83 GPM/ft3/s)*(8.33 lb/gal)*(60 min/hr)*(1 MBTU/1000000 BTU)*(Teff-Tup)
  heat_load_receiving = critQ_cfs * temp_receiving * 9.7
  heat_load_effluent= effluentQ_cfs * temp_effluent_limit * 9.7
  
  #calculate final combined temperature ((Qr*Tr)+(Qd*Td))/(Qr+Qd)
  combined_temp = ((critQ_cfs * temp_receiving) + (effluentQ_cfs * temp_effluent_limit)) / (comb_q)
    
  #calculate final temperature increase (Tcombined - Treceiving)
  t_increase = combined_temp - temp_receiving
  
  #check if temperature and temperature increase exceeds standards and note
  temperature_exceed = ifelse(combined_temp > temp_criterion, TRUE, FALSE)
  t_increase_exceed = ifelse(t_increase > delta_temp, TRUE, FALSE)
  
  #note and caveat
  # "Note: wasteload analysis may allow unreasonably high allowed temperature and heat loading.'
  # 'Narrative standards, new source performance standards, and BAT also apply, thus possibly'
  # 'reducing the values give in this wasteload analysis.'
  
  
  # Gather, return, & print results
  result=list(
	heat_load_receiving_init=heat_load_receiving_init,
	heat_load_effluent_init=heat_load_effluent_init,
	heat_load_receiving=heat_load_receiving,
	heat_load_effluent=heat_load_effluent,
	
	temp_criterion=temp_criterion,
	delta_temp=delta_temp,
	
	comb_q=comb_q,
	combined_temp_init=combined_temp_init,
	t_increase_init=t_increase_init,
	temp_effluent_increase_limit=temp_effluent_increase_limit,
	temp_effluent_temp_limit=temp_effluent_temp_limit,
	combined_temp=combined_temp,
	t_increase=t_increase,
	temperature_exceed=temperature_exceed,
	t_increase_exceed=t_increase_exceed,
	temp_effluent_limit=temp_effluent_limit
  )

  return(result)
}



