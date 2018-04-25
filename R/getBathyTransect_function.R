#' getBathyTransect
#'
#' This function returns a transect from a bathymetry object. if a bathymetry object hasn't been specified, it first querys the NOAA database to get one. If bathymetry data is provided but no start or end to the transect has been specified, an interactive map allows users to select transect start and end points
#' @param bathymetry An object of class Bathymetry as defiend by the marmap package. Defaults to NULL. If not specified, a bathymetry object will be called form transect parameters
#' @param startFence An vector containing the longitude and latitude points denoting the begining of the fence.
#' @param startFence An vector containing the longitude and latitude points denoting the end of the fence.
#' @keywords get bathymetry and transect
#' @export
#' @examples
#' getBathyTransect(startFence = c(-157.68330, 21.28333), endFence = c(-157.53330, 21.41667))


getBathyTransect = function(bathymetry = NULL, startFence = NULL, endFence = NULL){
  if(!is.null(startFence) & !is.null(endFence) & is.null(bathymetry)){
  ## If there is not a specified bathymetry, but there are specified fence starts and ends, download the bathymetry from NOAA
  if(is.null(bathymetry)){
    lon1 = startFence[1]; lon2 = endFence[1]; lat1 = startFence[2]; lat2 = endFence[2]
  }

    ## Adding a bit to each point so that bathymetry is larger than actual transect start and end
    if(lon1 > lon2 & lon1 > 0){
      lon1 = lon1 + 0.05
      lon2 = lon2 - 0.05
    } else if (lon1 > lon2 & lon1 < 0){
      lon1 = lon1 - 0.05
      lon2 = lon2 + 0.05
    } else if (lon1 < lon2 & lon1 > 0){
      lon1 = lon1 + 0.05
      lon2 = lon2 - 0.05
    } else if (lon1 < lon2 & lon1 < 0){
      lon1 = lon1 - 0.05
      lon2 = lon2 + 0.05
    }

    if(lat1 > lat2 & lat1 > 0){
      lat1 = lat1 + 0.05
      lat2 = lat2 - 0.05
    } else if (lat1 > lat2 & lat1 < 0){
      lat1 = lat1 - 0.05
      lat2 = lat2 + 0.05
    } else if (lat1 < lat2 & lat1 > 0){
      lat1 = lat1 - 0.05
      lat2 = lat2 + 0.05
    } else if (lat1 < lat2 & lat1 < 0){
      lat1 = lat1 + 0.05
      lat2 = lat2 - 0.05
    }
    ## Getting bathymetry from NOAA Servers
    bathymetry = getNOAA.bathy(lon1, lon2, lat1, lat2, resolution = 4)
  }

  ## Subsetting bathymetry to get a transect along which to place fence
  if(!is.null(startFence)){
    transect = get.transect(bathymetry, startFence[1], startFence[2], endFence[1], endFence[2], distance = TRUE)
  } else {
    transect = get.transect(bathymetry, locator = TRUE, distance = TRUE)
  }
  bathy_transect = list()
  bathy_transect$bathy = bathymetry
  bathy_transect$transect = transect
  return(bathy_transect)
}
