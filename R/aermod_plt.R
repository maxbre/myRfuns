#' plt_cols
#'
#' Return a column specification for use with [readr::read_fwf()].
#'
#' @param path a string for the path of PLT file
#' @param cols a character vector to select and optionally rename columns of the PLT file
#' @param skip number of rows to skip
#' @return a tibble, identical in structure to the return value of [readr::fwf_positions()]
#'

plt_cols <- function (path,
                      cols = c(X = "X", Y = "Y", CONC = "AVERAGE CONC", ID="NET ID"),
                      skip = 6
                      ) {

  # header row 
  header_fwf <- readr::fwf_empty(path, skip = skip)
  header_row <- readr::read_fwf(path, col_positions = header_fwf, skip = skip, n_max = 1)

  # name cols
  header_vars <- unname(header_row)
  
  # index of col positions
  i <- match(cols, header_vars)

  col_positions <-
    readr::fwf_positions(
      header_fwf$begin[i],      # subset by position i
      header_fwf$end[i],        # subset by position i
      col_names = names(cols))  # set column names

  # return col position
  col_positions
  
  }

#' Import AERMOD.PLT file as tabular data
#'
#' @param path a string for the path of PLT file
#' @param cols a character vector to select and optionally rename columns of the PLT file
#' @return an object of type tbl
#' @export
#' @examples \dontrun{}
#' 

read_plt_tbl<-function(path,
                       cols = c(X = "X", Y = "Y", CONC = "AVERAGE CONC", ID="NET ID")
                       ) {
  
  col_positions <-plt_cols(path, cols = cols)
  
  plt_data <-  vroom::vroom_fwf(path,
                                id=path,
                                skip = 8,
                                col_positions = col_positions
                                )
  }





