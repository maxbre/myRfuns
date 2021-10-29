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

