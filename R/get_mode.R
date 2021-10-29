#' Get the mode
#'
#' @param v a numeric vector
#'
#' @return get the mode (the most frequent value)
#' @export
#'
#' @examples
#' v<- c(NA, 19, 4, 5, 7, 29, 19, 29, 13, 25, 19, 19)
#' mode(v)

get_mode <- function(v) {

  # this is dealing with NA
  uniqv <- stats::na.omit(unique(v))

  uniqv[which.max(tabulate(match(v, uniqv)))]

}

