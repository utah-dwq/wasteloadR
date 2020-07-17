
#sites=readWQP(type='sites', siteid=c('UTAHDWQ_WQX-4990050','UTAHDWQ_WQX-4990060','UTAHDWQ_WQX-4990080'))
#test_data=readWQP(type='result', siteid=c('UTAHDWQ_WQX-4990050','UTAHDWQ_WQX-4990060','UTAHDWQ_WQX-4990080'))
#test_data=merge(test_data, sites)
#test=wasteloadR_stats(data=test_data, site_grps=list(upstream=c("UTAHDWQ_WQX-4990080"), downstream=c("UTAHDWQ_WQX-4990050","UTAHDWQ_WQX-4990060")))

wasteloadR_stats=function(data,
						grp_vars=c("MonitoringLocationIdentifier","MonitoringLocationName","SampleCollectionMethod.MethodName","CharacteristicName",
							"ResultSampleFractionText","ResultMeasure.MeasureUnitCode"),
						site_grps=NULL, seasons=list(spring=c(3,4,5), summer=c(6,7,8), fall=c(9,10,11), winter=c(12,1,2)), nd="half")
					{
	
	# Testing setup
	#grp_vars=c("OrganizationIdentifier","MonitoringLocationIdentifier","MonitoringLocationName","SampleCollectionMethod.MethodName","CharacteristicName",
	#					"ResultSampleFractionText","ResultMeasure.MeasureUnitCode")
	#stats=c('min','mean','median','max')
	#pctiles=c(0.2,0.8)
	#site_grps=list(upstream=c("UTAHDWQ_WQX-4990080"), downstream=c("UTAHDWQ_WQX-4990050","UTAHDWQ_WQX-4990060"))
	#seasons=list(spring=c(3,4,5), summer=c(6,7,8), fall=c(9,10,11), winter=c(12,1,2))
	#nd="half" #"det" "NA"
	#sites=readWQP(type='sites', siteid=c('UTAHDWQ_WQX-4990050','UTAHDWQ_WQX-4990060','UTAHDWQ_WQX-4990080'))
	#data=readWQP(type='result', siteid=c('UTAHDWQ_WQX-4990050','UTAHDWQ_WQX-4990060','UTAHDWQ_WQX-4990080'))
	
	
	#pastestats=function(x){
	#	paste0(x,"=",x,"(value, na.rm=T)")	
	#}
	#
	#stats2=sapply(stats, pastestats)	
	
	# Assign month
	data=within(data, {
		month=lubridate::month(ActivityStartDate)
	})
	
	# Assign seasons
	#if(missing(seasons)){print('seasons missing')}
	if(!is.null(seasons)){
		seasons=stack(seasons)
		names(seasons)=c("month","Season")
		data=merge(data, seasons, all.x=T)
		grp_vars=append(grp_vars, "Season")
	}
	
	# Assign site groups
	if(!missing(site_grps)){
		site_grps=stack(site_grps)
		names(site_grps)=c("MonitoringLocationIdentifier","SiteGroup")
		data=merge(data, site_grps, all.x=T)
		grp_vars=append(grp_vars, "SiteGroup")
	}
	
	# Fill non-detects
	if(! nd %in% c("half","det","NA")){stop("Error: nd argument must be one of 'half', 'det', or 'NA'")}
	data$value=data$ResultMeasureValue
	
	if(nd == "half"){
		data=within(data, {
			value=ifelse(is.na(value) & ResultDetectionConditionText=="Not Detected", DetectionQuantitationLimitMeasure.MeasureValue/2, value)	
		})
	}
	if(nd == "det"){
		data=within(data, {
			value=ifelse(is.na(value) & ResultDetectionConditionText=="Not Detected", DetectionQuantitationLimitMeasure.MeasureValue, value)	
		})
	}
	if(nd == "NA"){
		data=within(data, {
			value=ifelse(ResultDetectionConditionText=="Not Detected", NA, value)
		})
	}
	
	data$value=wqTools::facToNum(data$value)
	
	with(subset(data, ResultDetectionConditionText=="Not Detected"), {all(DetectionQuantitationLimitMeasure.MeasureUnitCode==ResultMeasure.MeasureUnitCode)})
	
	data=data[!is.na(data$value),]
	
	
	# Fill over-detects
	data=within(data, {
		value=ifelse(is.na(value) & ResultDetectionConditionText=="Present Above Quantification Limit", DetectionQuantitationLimitMeasure.MeasureValue, value)	
	})
	
	summary=data %>% 
		dplyr::group_by(.dots=lapply(grp_vars, as.symbol)) %>%
		#dplyr::summarize()
		dplyr::summarize(min=min(value, na.rm=T), median=median(value, na.rm=T), mean=mean(value, na.rm=T), max=max(value, na.rm=T), pctile20=quantile(value, 0.20, na.rm=T), pctile80=quantile(value, 0.80, na.rm=T), samp_count=dplyr::n())

	return(summary)

}


