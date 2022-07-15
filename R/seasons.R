#' Generate low flow statistics (nQy) for wasteloads
#'
#' @param dates Input date vector. Must of class "Date".
#' @param seasons Named list of seasons.
#' @return Returns a vector of season names associated with each date.
#' @importFrom lubridate month

#' @examples
#' dates=as.Date(c("2022-08-29","2022-09-30","2022-10-01","2022-10-02","2022-10-03"))
#' seasons(dates)
#' @export
seasons=function(dates, seasons=list("winter"=c(1,2,3), "spring"=c(4,5,6), "summer"=c(7,8,9), "fall"=c(10,11,12))){

	if(class(dates) != "Date"){
		stop("Input must be date class.")
	}

	seasons_df=stack(seasons)
	names(seasons_df)=c("month","season")
	date_month=data.frame(dates, lubridate::month(dates))
	names(date_month)=c("date","month")

	month_seasons=dplyr::left_join(date_month, seasons_df, by = "month")

	return(month_seasons$season)
}




