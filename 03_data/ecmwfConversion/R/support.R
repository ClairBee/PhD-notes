
########################################################################################################
# CONVERSION TOOLS                                                                                  ####

#' Offset-adjust a forecast array
#' 
#' Take an array of forecasts organised by forecast date, and return organised by realisation date.
#' 
#' @param arr Array of forecasts to be adjusted
#' 
#' @return Array of adjusted forecasts
#' 
#' @export
#' 
os.correct <- function(arr) {
    
    if (!is.null(attributes(arr)$type)) {
        if (attributes(arr)$type == "offset-adjusted") {
            cat("This array is already adjusted by leadtime.")
        }
    } else {
        # convert leadtimes to days
        dimnames(arr)$step <- as.integer(dimnames(arr)$step) / 24
        
        # create array to hold output
        adj <- arr[,(length(dimnames(arr)$step)+1):ncol(arr),,,,,]
        
        invisible(sapply(dimnames(arr)$step, function(lt) {
            adj[,,,lt,,,] <<- arr[,(length(dimnames(arr)$step)+1):ncol(arr) - as.integer(lt),,lt,,,]
        }))
        
        # label as offset-adjusted for the avoidance of doubt
        attr(adj, "type") <- "offset-adjusted"
        
        return(adj)
    }
}

