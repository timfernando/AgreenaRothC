#' @title Yield to residues
#' @description Calculate crop specific biomass residues
#' baseline and scenario runs.
#' @name yield_to_resid
#' @param yiled vector with crop yields
#' @param cropname vector with cropnames
#' @return vector with crop residues in t C/ha
#' @author Marcos Alves
#' @export

yield_to_resid <- function(yield, cropname) {
  # cfg <- CFGs[cropname, 1]
  coefs <- yld2bio[cropname, ]
  bio_inputs <-
    (yield * coefs[, "Dry.matter.fraction.FDM"] * coefs[, "Slope.a"] + coefs[, "Intercept.b"]) * 0.15 # CFT considers a fixed amount of carbon per copr type
  return(bio_inputs)
}
