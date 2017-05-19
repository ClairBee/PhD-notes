
#' Inspect netcdf file
#' 
#' Inspect a .nc file and print a list of the variables and dimensions.
#' 
#' @param fnm Filename of netcdf object to inspect (including path)
#' @param detail Boolean: report only names of dimensions (F) or their values (T)? Default is to show all values.
#' 
#' @export
#' 
nc.inspect <- function(fnm, detail = T) {
    
    fnm <- paste0(gsub(".nc$", "", fnm), ".nc")
    
    if(!file.exists(fnm)) {
        cat("No such file exists. \n")
    } else {
        nc <- nc_open(fnm)
        
        cat("\n")
        print(data.frame("Variable        " = names(nc$var), 
                         "Definition                    " = sapply(nc$var, "[[", "longname"), 
                         "Unit" = sapply(nc$var, "[[", "units")), right = F, row.names = F)
        
        if(detail) {
            dd <- which(names(nc$dim) == "date")
            cat("\n Dimensions: \n")
            print(sapply(nc$dim[-dd], "[[", "vals"))
            cat("$date \n")
            print(as.Date(nc$dim[[c(dd)]]$vals, origin = "1900-01-01"))
        } else {
            cat("\n Dimensions: ", names(nc$dim))
        }
        nc_close(nc)
    }
}



#' Load netcdf file into array
#' 
#' Import the contents of a .nc file into an array of the specified dimensions
#' 
#' @param fnm Filename of netcdf object to import (including path)
#' @param lt.as.days Boolean: return forecast step/leadtime in hours (F) or days (T)? Default is hours)
#' 
#' @return Array of same dimensions as source data, with all dimensions labelled according to the netcdf specifications.
#' 
#' @export
#' 
nc.array <- function(fnm, lt.as.days = F) {
    
    fnm <- paste0(gsub(".nc$", "", fnm), ".nc")
    
    if(!file.exists(fnm)) {
        cat("No such file exists. \n")
    } else {
        
        # open link to netcdf file
        nc <- nc_open(fnm)
        
        # extract variables into array
        arr <- abind(invisible(sapply(names(nc$var), function(varb) ncvar_get(nc, varb), simplify = F)),
                     along = 0)
        
        # rename all axes
        dimnames(arr)[-1] <- sapply(names(nc$dim), function(dm) ncvar_get(nc, dm))
        names(dimnames(arr)) <- c("varb", names(nc$dim))
        dimnames(arr)$date <- gsub("-", "", as.Date(as.integer(dimnames(arr)$date), origin = "1900-01-01"))
        
        # relabel steps as days if preferred
        if (lt.as.days) dimnames(arr)$step <- as.integer(dimnames(arr)$step) / 24
        
        # reorder lat & long into ascending order
        arr <- arr[,order(ncvar_get(nc, "longitude")),order(ncvar_get(nc, "latitude")),,,, drop = F]
        
        # close link to netcdf file & return array
        nc_close(nc)
        return(arr)
    }
}