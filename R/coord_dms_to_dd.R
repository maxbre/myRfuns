#' coord_dms_to_dd
#' 
#' Degrees minutes seconds to decimal degrees
#'
#' @param s a vector of strings in the form "degrees minutes seconds" separated by a blank space
#' @return a numeric vector in decimal degrees
#' @export
#' @examples
#' myangle<- "45  12    3.01"
#' coord_dms_to_dd(myangle)

coord_dms_to_dd <- function(s) {

  x <- do.call(rbind, strsplit(s, split='\\s+'))
  #x <- do.call(rbind, strsplit(se, split="[[:space:]]+"))

  x <- apply(x, 1, function(y) {

    y <- as.numeric(y)

    y[1] + y[2]/60 + y[3]/3600

    })

  x
}



