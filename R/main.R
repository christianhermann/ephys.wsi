#' Loads and Downloads all packages need and used and some more
#'
#' @return
#' @export
#'
#' @examples
prepare_packages <- function(libpath = NULL) {

  if(!require(checkpoint)) install.packages("checkpoint")
  if(!require(checkpoint)) install.packages("checkpoint")

  checkpoint("2022-04-07", scanForPackages = F)
  check_packages(

    c(
      "dplyr",
      "tidyverse",
      "stringi",
      "openxlsx",
      "gWidgets2",
      "matrixStats",
      "checkmate",
      "outliers",
      "purrrlyr",
      "ggsci",
      "ggthemes",
      "factoextra",
      "ggplot2",
      "patchwork",
      "cluster",
      "factoextra",
      "ggalt",
      "gtools",
      "latex2exp",
      "cowplot",
      "scales",
      "svglite",
      "Cairo",
      "gridExtra",
      "gWidgets2tcltk",
      "ggpubr"),

    libpath
  )
}
