#' Angle degrees to radians
#'
#' @param deg a numeric vector of angles degrees
#'
#' @return a numeric vector of corresponding radians
#' @export
#'
#' @examples
#' deg<- c(0, 30, 45, 60, 90, 180, 270, 360)
#' deg2rad(deg)

deg2rad <- function(deg) {(deg * pi) / (180)}

#' Radians to angle degrees
#' #'
#' @param rad a numeric vector of radians
#'
#' @return a numeric vector of corresponding angle degrees
#' @export
#'
#' @examples
#' rad<-c(0, pi/6, pi/4, pi/3, pi/2, pi, 3/2*pi, 2*pi)
#' rad2deg(rad)

rad2deg <- function(rad) {(rad * 180) / (pi)}




