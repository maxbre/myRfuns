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

#' wind dir convert sectors to degrees
#'
#' @param vs a character vector of wind dir sectors
#'
#' @return a numeric vector of wind dir degrees
#' @export
#'
#' @examples
#' vs<-c("N","NNE","NE", "E", "ESE", "SE", "SSE", "S", "W", "WNW", "NW", "NNW")
#' sect_to_degr(vs)

sect_to_degr<-function(vs){

  # vs vector of sectors

  # degrees (the centre of the corrisponding sector)
  degr<-seq(0, 337.5 , by = 22.5)

  # sectors (pay attention here it is a different vector than in the function degr_to_sect)
  sect<-c("N",
          "NNE","NE", "ENE", "E",
          "ESE", "SE", "SSE", "S",
          "SSW", "SW", "WSW", "W",
          "WNW", "NW", "NNW")

  # define the lookup named vector
  lkup_sect<-stats::setNames(degr, sect)

  # lookup at the sectors
  vs<-lkup_sect[vs]

  # return vector of sectors (characters)
  unname(vs)

}


