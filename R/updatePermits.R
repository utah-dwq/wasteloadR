#' Update permit locations
#'
#' This function updates permit locations in the local install of wasteloadR. Queries permit locations from EPA ECHO. Primarily for use by package developers.
#' @import dplyr
#' @importFrom wqTools readECHO_fac
#' @examples 
#' updatePermits()
#' @return Exports a .rdata object of permit identifiers and spatial coordinates to the extdata folder of wasteloadR.
#' @export
updatePermits=function(){
	permits=wqTools::readECHO_fac(p_st="ut", p_act="y")	
	permits$properties$prefix=substr(permits$properties$SourceID, 1, 3)
	permits=subset(permits, properties$prefix %in% c('UT0'))
	permits_coords=do.call(rbind.data.frame,permits$geometry$coordinates)
	names(permits_coords)=c("dec_long","dec_lat")
	permits_coords=data.frame(permits$properties[,c("SourceID","CWPName")], (permits_coords))
	names(permits_coords)[names(permits_coords)=="SourceID"]="permit_id"
	names(permits_coords)[names(permits_coords)=="CWPName"]="permit_name"
	names(permits_coords)[names(permits_coords)=="dec_long"]="LongitudeMeasure"
	names(permits_coords)[names(permits_coords)=="dec_lat"]="LatitudeMeasure"
	#path=paste0(path.package('wasteloadR'),'/extdata')
	#save(permits_coords, file=paste0(path,"/permits.Rdata"))
	permits_coords=permits_coords %>% wqTools::assignPolys(wqTools::au_poly) %>% wqTools::assignPolys(wqTools::bu_poly) %>% wqTools::assignPolys(wqTools::wmu_poly)
	permits_coords=permits_coords[,!names(permits_coords) %in% "polyID"]
	save(permits_coords, file="C:\\Users\\jvander\\Documents\\R\\wasteloadR\\data\\permits_coords.Rdata")	
}

