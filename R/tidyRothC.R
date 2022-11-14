#' Auxiliary function to transform RothC individual results in tidy long data frames.
#' @title Tidy AgreenaRothC outputs
#' @param ... list of objects output from \link{AgreenaRothC}
#' @return Long formatted soil carbon values that can be ploted by \link{plotRothC}
#' @details Receives as inputs a list of data frames containing the output of AgreenaRothC and clean the data by keeping only the soil carbon stocks values and tranforming it from wide to long with a names for each datafram inputed.
#' @author Marcos Alves
#' @seealso \link{AgreenaRothC}, \link{plotRothC}
#' @import tidyr
#' @export
#'

tidyRothC <- function(...) {
  x <- list(...)
  z <- NULL
  for(i in 1:nargs()) {
    y <- x[[i]]["soilC_scenario"][[1]]
    y <- y[,grepl("SC", colnames(y))]
    colnames(y) <- c(paste0("SC_M_",i), paste0("SC_SD_",i))
    z <- c(z,y)
  }
  z <- data.frame(z, "month" = 1:nrow(y))
  z <- pivot_longer(z, cols = grep("SC_M",colnames(z), value = TRUE), names_to = c("run_code_mean"), values_to = "mean") %>%
    pivot_longer(cols = grep("SC_SD",colnames(z), value = TRUE), names_to = c("run_code_sd"), values_to = "sd")

  w <- NULL
  for(i in 1:nargs()) {
    mn <- x[[i]]["Mean_capture"][[1]]
    sd <- x[[i]]["Std_dev"][[1]]

    y <- y[,grepl("SC", colnames(y))]
    colnames(y) <- c(paste0("SC_M_",i), paste0("SC_SD_",i))
    w <- c(w,y)
  }
  w <- data.frame(w,"month" = 1:nrow(y))
  w <- pivot_longer(w, cols = grep("SC_M",colnames(w), value = TRUE), names_to = c("run_code_mean"), values_to = "mean") %>%
    pivot_longer(cols = grep("SC_SD",colnames(w), value = TRUE), names_to = c("run_code_sd"), values_to = "sd")
  return(list("scenario" = z, "baseline" = w))
}

