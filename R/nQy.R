#' Generate low flow statistics (nQy) for wasteloads
#'
#' @param data Input data. Should be a dataframe of daily discharge values
#' @param n Numeric. Days for rolling time period averages. Default 7.
#' @param y Numeric. Recurrence interval in years
#' @param date_col Column name containing date values. Must be in standard "YYYY-MM-DD" date format.
#' @param q_col Column name containing discharge values. Any appropriate discharge units are OK. The result will be in the same units as the input.
#' @param min_obs Minimum number of daily observations to allow an n-day average to be calculated. Default 1. Passed to roll::roll_mean for n-day average calculations. See ?roll::roll_mean for more information regarding usage.
#' @param out_type One of nQy for low flow statistic output or min_ann_q to see the minimum annual values of n-day average discharges.
#' @return Either a numeric value of nQy or a dataframe of minimum annual n-day average discharge values.
#' @import dplyr
#' @importFrom lubridate year
#' @importFrom roll roll_mean
#' @examples
#' # Basic usage
#' ## Get data
#' library(dplyr)
#' gauge_data=dataRetrieval::readNWISdv(siteNumbers="10171000", startDate="2000-01-01", parameterCd="00060") %>% dplyr::rename(discharge_cfs=X_00060_00003, disch_code=X_00060_00003_cd)# Parameter code 00060 = discharge in cfs. see also ?readNWISuv() to read high frequency values
#' ### Also try wqTools::findSites() to find other gauge locations.
#' 
#' ## Run the function
#' JR_1700S_7Q10=nQy(data=gauge_data, n=7, y=10, date_col="Date", q_col="discharge_cfs")
#' JR_1700S_7Q10
#' 
#' ## Get min annual flows instead of nQy result
#' JR_1700S_min_ann_Q=nQy(data=gauge_data, n=7, y=10, date_col="Date", q_col="discharge_cfs", out_type="min_ann_q")
#' JR_1700S_min_ann_Q
#' 
#' ## Other nQy types
#' JR1700S_30Q2=nQy(data=gauge_data, n=30, y=5, date_col="Date", q_col="discharge_cfs")
#' JR1700S_30Q2
#' 
#' # Using plyr::ddply to perform advanced analyses (multiple sites, seasonal, etc.)
#' 
#' ## Multiple sites
#' gauge_data=dataRetrieval::readNWISdv(siteNumbers=c("10171000","10168000","10170500"), startDate="2000-01-01", parameterCd="00060") %>% dplyr::rename(discharge_cfs=X_00060_00003, disch_code=X_00060_00003_cd)# Parameter code 00060 = discharge in cfs. see also ?readNWISuv() to read high frequency values
#' multi_site_7Q10=plyr::ddply(gauge_data, .variables="site_no", .fun="nQy") # Just default arguments to nQy here which generates 7Q10
#' multi_site_7Q10
#' 
#' ## Split by season
#' gauge_data=dataRetrieval::readNWISdv(siteNumbers=c("10171000","10168000","10170500"), startDate="2000-01-01", parameterCd="00060") %>% dplyr::rename(discharge_cfs=X_00060_00003, disch_code=X_00060_00003_cd)# Parameter code 00060 = discharge in cfs. see also ?readNWISuv() to read high frequency values
#' gauge_data$month=lubridate::month(gauge_data$Date)
#' seasons=data.frame(c("Summer","Summer","Summer", "Fall", "Fall", "Fall", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring"), c(7,8,9,10,11,12,1,2,3,4,5,6))
#' names(seasons)=c("season", "month")
#' gauge_data=merge(gauge_data, seasons, all.x=T)
#' 
#' multi_site_seasonal_1Q10=plyr::ddply(gauge_data, .variables=c("site_no","season"), .fun="nQy", n=1, y=10) # Note additional arguments to ddply will be passed to the nQy function
#' multi_site_seasonal_1Q10


#' @export
nQy = function(data, n=7, y=10, date_col="Date", q_col="discharge_cfs", min_obs=1, out_type="nQy"){

	# Generate quantile from y
	qtile=1/y
	
	# Reduce columns
	data=data[,c(date_col, q_col)]
	names(data)=c("date","q")
	
	# Check for multiple q values on single date ,summarize as needed
	if(any(table(data$date)>1)){
		warning("Multiple discharge values detected for one or more dates. The mean value for each date used in rolling mean calculations.")
		data=data %>% dplyr::group_by(date) %>% dplyr::summarize(q=mean(q), .groups="drop")
	}

	# Check for implicit NAs
	all_dates=data.frame(seq(min(data$date), max(data$date), 1))
	names(all_dates)="date"
	data=merge(data, all_dates, all.x=T)
	if(any(is.na(data$q))){
		warning("NA discharge values detected. Rolling means will calculated across NAs for periods meeting min_obs requirement. See ?roll::roll_mean for more information regarding min_obs argument.")
	}
		
	# Calculate rolling means
	rolled_mean=data.frame(roll::roll_mean(data$date, width=n), roll::roll_mean(data$q, width=n, min_obs=min_obs))
	names(rolled_mean)=c("date","q")
	rolled_mean=subset(rolled_mean, !is.na(rolled_mean$date))	
	
	# Assign year
	rolled_mean$year=lubridate::year(rolled_mean$date)
	
	# Calculate minimum annual flows
	
	min_ann_q = rolled_mean %>% dplyr::group_by(year) %>% dplyr::summarize(min_q=min(q), .groups="drop")
	
	
	# Get nQy result
	
	nQy=quantile(min_ann_q$min_q, qtile)

	# Return results
	if(out_type=="min_ann_q"){
		return(list("nQy"=nQy[[1]], "min_ann_q"=as.data.frame(min_ann_q)))
	}else{
		return(nQy[[1]])
	}
}

