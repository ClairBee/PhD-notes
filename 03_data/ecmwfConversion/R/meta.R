
#' Quickly refresh & reinstall packages
#' @export
#' 
refresh <- function() {
    require(devtools)
    require(roxygen2)
    org.dir <- getwd()
    target.dir <- paste0("/home/clair/Documents/PhD/03_data/ecmwfConversion")
    
    if (org.dir != target.dir) {setwd(target.dir)}
    document()
    setwd("..")
    install("ecmwfConversion")
    setwd(org.dir)
}