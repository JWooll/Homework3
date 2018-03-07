
#' Max Min and Mean of single Column of Data
#'
#' Uses Max and min function after selecting a specific
#' column of data
#'
#'@param data Data set being used
#'@param zed column name (in quotations)
#'@return list of selected column, max value and min value in that order
#'@export
Tools_margin <- function(dat, zed){
  np <- dplyr::select(dat,zed)
  mx <- max(np,na.rm = T)
           mn <- min(np,na.rm = T)
           return(c(np,mx,mn))
           }

