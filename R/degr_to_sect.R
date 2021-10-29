#' wind dir convert degrees to sectors
#'
#' @param vd a numeric vector of wind dir degrees
#'
#' @return a character vector of wind dir sectors
#' @export
#'
#' @examples
#' vd<-c(0, seq(11.25, (360-11.25), by = 22.5), 360)
#' degr_to_sect(vd)

degr_to_sect<-function(vd){

  # vd as vector of degrees

  #define the breaks
  breaks<-c(0, seq(11.25, (360-11.25), by = 22.5), 360)

  # define the labels
  labels<-c("N",
            "NNE", "NE", "ENE", "E",
            "ESE", "SE", "SSE","S",
            "SSW","SW","WSW","W",
            "WNW","NW", "NNW",
            "N")

  # cut by predefined breaks and return vector of sectors (labels)
  # pay attention here to the inclusion of interval limits

  vd<-cut(vd,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE)

  as.character(vd)
}


