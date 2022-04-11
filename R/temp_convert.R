#' Temp conversion from Celsius degrees °C to ...
#'
#' @param deg a numeric vector of Celsius degrees
#' @param to  an user input string for the conversion: "K" for Kelvin (default), "F" for Fahrenheit degrees
#'
#' @return a numeric vector of the converted temperature degrees
#' @export
#'
#' @examples
#' C_convert(0, "K")
#' C_convert(1, "F")

C_convert <- function(deg = 0, to = "K") {

  switch(tolower(to),
         "k" = deg + 273.15,
         "f" = (deg * 1.8) + 32
         )
  }


#' Temp conversion from Fahrenheit degrees F to ...
#'
#' @param deg a numeric vector of Fahrenheit degrees
#' @param to  an user input string for the conversion: "K" for Kelvin (default), "C" for Celsius degrees
#'
#' @return a numeric vector of the converted temperature degrees
#' @export
#'
#' @examples
#' F_convert(0, "K")
#' F_convert(1, "C")

F_convert <- function(deg = 0, to = "K") {

  switch(tolower(to),
         "k" = (deg - 32) / 1.8 + 273.15,
         "c" = (deg - 32) / 1.8
         )
  }

