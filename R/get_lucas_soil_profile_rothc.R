#' @export

get_lucas_soil_profile_rothc <- function(lonlat) {
  soil <- data.frame(label = c("0-10cm", "10-20cm", "20-30cm"), Carbon = 1:3, ParticleSizeClay = 1:3)
  distancia <- function(x1, x2) {
    return(dist(rbind(x1, x2)))
  }
  y <- apply(lucas[, c("TH_LONG", "TH_LAT")], 1, distancia, lonlat)
  lucas_soil <- lucas[match(min(y), y), c("Depth", "OC", "CLAY")]
  carbon <- rep(sum(as.numeric(lucas_soil$OC) / (2 / 3)), 3) # assuming carbon density is constant between 0-20 and 20-30 cm and extrapolating (regra de 3)
  soil$ParticleSizeClay <- rep(lucas_soil$CLAY, 3)
  soil$Carbon <- carbon
  attr(soil, "meta")$Longitude <- lucas[match(min(y), y), c("TH_LONG")]
  attr(soil, "meta")$Latitude <- lucas[match(min(y), y), c("TH_LAT")]
  return(soil)
}
