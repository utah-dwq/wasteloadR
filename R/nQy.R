#' Generate low flow statistics (nQy) for wasteloads
#'
#' Calculates n-day rolling averages & minimum annual n-day averages, fits a probability distribution to minimum annual average flows, and calculates low flow statistics (nQy) for discharge data.
#' @param data Input data. A dataframe columns of daily discharge values and dates.
#' @param n Numeric. Days for rolling time period averages. Default 7.
#' @param y Numeric. Recurrence interval in years
#' @param date_col Column name containing date values. Must be in standard "YYYY-MM-DD" date format.
#' @param q_col Column name containing discharge values. Any appropriate and uniform discharge units are OK. The result will be in the same units as the input.
#' @return Returns a list of results including the calculated nQy statistics (from fitted probability and 1/y percentile), a data frame of annual n-day flow minima and distribution information, and a plot to examine the fit.
#' @import dplyr
#' @importFrom lubridate year
#' @importFrom zoo rollmean
#' @importFrom moments skewness
#' @import ggplot2
#' @examples
#' # Basic usage
#' ## Get data
#' library(dplyr)
#' gauge_data=dataRetrieval::readNWISdv(siteNumbers="10171000", startDate="2000-01-01", parameterCd="00060") %>% dplyr::rename(discharge_cfs=X_00060_00003)# Parameter code 00060 = discharge in cfs.
#' ### Also try wqTools::findSites() to find other gauge locations.
#' 
#' ## Run the function
#' JR_1700S_7Q10=nQy(data=gauge_data, n=7, y=10, date_col="Date", q_col="discharge_cfs")
#' JR_1700S_7Q10
#' 
#' ## Other nQy types
#' JR1700S_1Q10=nQy(data=gauge_data, n=1, y=10, date_col="Date", q_col="discharge_cfs")
#' JR1700S_1Q10
#' 

#' @export
nQy = function(data, n=7, y=10, date_col="Date", q_col="discharge_cfs", plot_fit=TRUE){

	# Function outline:
	## Data checks - multiple values on 1 day, implicit NAs
	## Calculate n-day rolling means
	## Calculate min annual n-day rolling mean
	## Rank min annual means and calculate return intervals and exceedance probs
	## Fit Pearson III distribution (or get percentile or some other approach)
	## Plot fitted values
	## Calculate nQy
	## Export results

	# Development set up
	#gauge_data=dataRetrieval::readNWISdv(siteNumbers=c("10141000"), startDate="1970-01-01", parameterCd="00060") %>% dplyr::rename(discharge_cfs=X_00060_00003, disch_code=X_00060_00003_cd)
	#data=gauge_data
	#date_col="Date"
	#q_col="discharge_cfs"
	#n=7
	#y=10
	#plot_fit=TRUE
	
	# Reduce columns
	data=data[,c(date_col, q_col)]
	names(data)=c("date","q")

	if(class(data$date) != "Date"){
		stop("Input date column must be date class.")
	}

	
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
		warning("NA discharge values detected. Rolling means will calculated across NAs.")
	}
	
	# Calculate rolling means
	rolled_means = data %>% mutate(ndaymean = zoo::rollmean(q, n, fill = NA, na.rm = F, align = "right")) 

	# Calculate min annual Q values
	q_ann_mins <- rolled_means %>% 
						mutate(year = wasteloadR::waterYear(date)) %>%
                        group_by(year) %>%
                        summarize(minQ = min(ndaymean, na.rm = T), 
                                  lenDat = length(q),
                                  lenNAs = sum(is.na(ndaymean))) %>%
                        filter(lenDat > 328 & lenNAs / lenDat < 0.1) # Only include years missing less than 10% of each year and 10% or fewer NAs
	
	# Check for removed years
	all_years=unique(wasteloadR::waterYear(data$date))
	included_years=unique(q_ann_mins$year)
	removed_years=all_years[!all_years %in% included_years]
	
	# Fill zeros in q_ann_mins (need to test w/ Weber River site)
	q_ann_mins$minQ_zerofilled=q_ann_mins$minQ
	q_ann_mins$minQ_zerofilled[q_ann_mins$minQ_zerofilled==0]=min(q_ann_mins$minQ[q_ann_mins$minQ!=0])/2 # Zeros filled w/ half of non-zero mins
	
	# add rank column and return interval column
	q_ann_mins <- q_ann_mins %>% 
                mutate(rank = rank(minQ_zerofilled, ties.method = "first")) %>%
                mutate(ReturnInterval = (length(rank) + 1)/rank) %>%
                mutate(ExceedProb = 1 / ReturnInterval)

	# Fit probability distribution
	## Measures of the distribution
	Xbar <- mean(log10(q_ann_mins$minQ_zerofilled))
	S    <- sd(log10(q_ann_mins$minQ_zerofilled))
	g    <- moments::skewness(log10(q_ann_mins$minQ_zerofilled))
	
	## Calculate z, K, to plot the fitted Pearson Type III
	q_ann_mins <- q_ann_mins %>% 
	mutate(z = 4.91 * ((1 / ReturnInterval) ^ 0.14 - (1 - 1 / ReturnInterval) ^ 0.14)) %>%
	mutate(K = (2 / g) * (((1 + (g * z) / 6 - (g ^ 2) / 36) ^ 3) - 1) ) %>%
	mutate(Qfit = 10^(Xbar + (K * S)))
	
	## Plot fitted distribution
	fit_plot=q_ann_mins %>% 
		ggplot(aes(x = ReturnInterval, y = minQ_zerofilled, color = "Estimated"))+
		geom_point()+
		geom_line(aes(x = ReturnInterval, y = Qfit, color = "Fitted"))+
		theme_classic()+
		scale_x_log10()+
		ylab("n day yearly minimum")+
		xlab("Return interval")
	
	if(plot_fit){plot(fit_plot)}
	
	# Calculate nQy stats
	z    <- 4.91 * ((1 / y) ^ 0.14 - (1 - 1 / y) ^ 0.14)
	K    <- (2 / g) * (((1 + (g * z) / 6 - (g ^ 2) / 36) ^ 3) - 1) 
	nQy_fitted <- 10^(Xbar + K * S)
	
	nQy_pctile=quantile(q_ann_mins$minQ, (1/y))
	
	# Print results
	print(paste0("Water years excluded from analysis due insufficient data: "))
	print(removed_years)
	print(paste0("Number of zero minimum annual flow values: ", dim(subset(q_ann_mins, minQ==0))[1]))
	print(paste0("Low flow statistic ", n, "Q", y, " fitted: ", round(nQy_fitted,2)))
	print(paste0("Low flow statistic ", n, "Q", y, " percentile: ", round(nQy_pctile,2)))

	# Return results
	results=list("nQy_fitted"=nQy_fitted, "nQy_pctile"=nQy_pctile, "fit_plot"=fit_plot, "annual_mins_fit"=q_ann_mins[order(q_ann_mins$rank),])
	return(results)
	
}

