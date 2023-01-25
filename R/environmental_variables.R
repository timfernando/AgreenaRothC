#' @title Environmental Variables
#' @description Download and preprocess environmental variables.
#' @name environmental_variables
#' @param long Longitute
#' @param lat Latitude
#' @param soil_depth Soil depth
#' @author Marcos Alves
#' @export
#'

environmental_variables <-
  function(lat, long, soil_depth) {

    # Climate-normal time span
    wth_dates <- c("1991-01-01", "2021-12-30")
    # as.numeric(format(as.Date(wth_dates[1]),'%Y')) - as.numeric(format(as.Date(wth_dates[2]),'%Y'))
    soil <-
        get_isric_soil_profile_rothc(c(long, lat),
                                     statistic = "mean",
                                     find.location.name = FALSE)
    soil_convert <- function(soil) {
      soil_av <- apply(soil[1:3,2:4], 2, weighted.mean, c(5,10,soil_depth - 15)) # find a literature-backed equation to take the correct average
      return(soil_av)
    }

    soil_av <- soil_convert(soil)
    wth <-
        get_wth_power_nasa(lonlat = c(attr(soil, "meta")$Longitude,
                                      attr(soil, "meta")$Latitude),
                           dates = wth_dates)
    data_lables <- c(names(soil_av),names(wth[,c(2,4,6)]))
    res <- array(data = NA, dim = c(1, 12, length(data_lables)), dimnames = list(1,  month.name, data_lables))
    for (i in names(soil_av)) {
      res[,,i] <- soil_av[i]
    }
    for (i in names(wth[,c(2,4,6)])) {
      res[,,i] <- t(wth[,i])
    }
    return(res)
  }
