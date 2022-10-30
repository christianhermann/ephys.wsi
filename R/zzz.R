.onLoad <- function(libname, pkgname) {

}

.onAttach <- function(libname, pkgname) {


  #####Ramp Settings####
  settings_envir$voltage_unit <- "V"
  settings_envir$step <- 0.0001
  settings_envir$ramp_data <- c(-0.1,0.1)
  settings_envir$asc_columns <- c("Index","Time[s]", "CurrentIn[A]","Potential[V]")
  packageStartupMessage(unlist(sapply(ls(settings_envir), function(x) paste0(x,": ",paste0(settings_envir[[x]],"\n",collapse = ",")))))
  packageStartupMessage("\n\n\n\nephys.WSI\nBy Christian Hermann \nAlways check and control your Results!\nNo guarantees for anything...")
}
