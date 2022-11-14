#' @title Plot AgreenaRothC diagnostics plots
#' @param x Results to be plotted (either "scenario" or "baseline")
#' @param ... list of objects output from \link{AgreenaRothC}
#' @return Long formatted soil carbon values that can be ploted by \link{plotRothC}
#' @details Receives as inputs a list of data frames containing the output of AgreenaRothC and plots diagnostics graphs comparing different runs
#' @author Marcos Alves
#' @seealso \link{AgreenaRothC}
#' @import ggplot2 ggpubr
#' @export
#'

plotRothC <- function(x = "scenario", ...){
  z <- tidyRothC(...)
  p1 <- ggplot(data = z[[x]], aes(x = month)) +
    # geom_line(aes(y = mean, color = run_code_mean ), size = 0.5) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = run_code_mean), alpha = .2) +
    geom_smooth(aes(y = mean,color = run_code_mean), size = 0.5)

  p2 <- ggplot(data = z[[x]], aes(x = run_code_mean, y = mean)) +
    geom_boxplot(aes(fill = run_code_mean), alpha = 0.9) +
    geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.7, alpha = 0.2, dotsize = 0.05)
  figure <- ggarrange(p1, p2,
                      labels = c("A", "B"),
                      ncol = 2, nrow = 1)
  print(figure)
}
