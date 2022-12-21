ctys <- data.frame(cty = c("RO", "DK", "LV", "UK", "MD", "EE", "UA", "BG", "LT", "ES", "PT"),
                   lon = c(27.533058,9.019888,25.278699,-1.335097,29.092413, 25.610963, 30.638949, 25.418037,24.219546,-4.477876, -7.473228),
                   lat = c(45.253864,55.684085,56.892089,53.135668,47.263174,58.774948,47.772385,43.253110,55.254268,38.321083,38.814869))
dsw <- list()
for (i in ctys$cty) {
  dsw[[ctys[ctys$cty == i,1]]]  <-
    get_wth_power_nasa(lonlat = c(ctys[ctys$cty == i,2], ctys[ctys$cty == i,3]),
                       dates = wth_dates)
}
usethis::use_data(dsw)
