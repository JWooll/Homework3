
#' Wrapped ggplot2
#'
#' Uses the ggplot function with na data points eliminated
#' User must call the function with the data set being used
#' (in this case the user will probably use the included data set AAF)
#' The X column they would like to use and the Y column they would like to use
#' Both need to be in quotes
#' and finally the type of graph they would like to make
#' eg. 'geom_point()','geom_jitter()','geom_line()' etc.
#'
#' @param dat,xc,yc,G type
#' @return ggplot of params
#' @export
Tools_gg <- function(dat,xc,yc,G){
  plot <- ggplot(data = dat, aes_string(x = xc, y = yc,na.rm = T)) +
           G
  return(plot)
           }
