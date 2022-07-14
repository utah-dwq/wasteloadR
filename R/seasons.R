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
	month=data.frame(lubridate::month(dates))
	names(month)="month"
	month_seasons=merge(month, seasons_df)
	
	season=month_seasons$season
	return(season)
}

