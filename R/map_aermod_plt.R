#' Convert mapview object to html and png files
#'
#' @param my_mapview a mapview object
#' @param string_filename_html a string to name the output html file
#'
#' @return a map in the form html and png
#' @export
#'
#' @examples \dontrun{}

save_mapview_html<-function(my_mapview, string_filename_html){

  # string date
  my_date<-format(Sys.Date(), "%Y%m%d")

  ## compose filename html
  my_filename<-paste0(my_date, '_', string_filename_html, '.html')

  #create both html and png, and remove some controls in png
  mapview::mapshot(my_mapview,
                   url = my_filename,
                   file=paste0(my_date, '_', string_filename_html, '.png'),
                   remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"))

  my_mapview

}

#' Create mapview object from aermod plt file, save results as html and png files
#'
#' @param input_filename a string for the aermod input file name *.plt
#' @param rows_to_skip_head a number of rows to skip in the head of the aermod plt file
#' @param rows_to_skip_tail a number of rows to skip in the tail of the aermod plt file, default = 0
#' @param trans_factor a number, transformation factor for results, default = 1
#' @param name_of_map_layer a string
#' @param string_filename_html a string
#'
#' @return a map in the form html and png
#' @export
#'
#' @examples \dontrun{}



plot_aermod_map<-function(input_filename,
                          rows_to_skip_head,
                          rows_to_skip_tail=0,
                          trans_factor=1,
                          name_of_map_layer,
                          string_filename_html)  {

  x <- y <- z <- NULL

  rows_file<-length(readLines(input_filename))
  rows_to_read<-rows_file-(rows_to_skip_head+rows_to_skip_tail)



  plt <- readr::read_table(input_filename,
                          col_names=FALSE,
                          skip=rows_to_skip_head,
                          n_max = rows_to_read)

  plt <- dplyr::select(plt, x=1, y=2, z=3)

  plt <- dplyr::mutate(plt, z=z*trans_factor)

  # coordinates to define a spatial point data frame
  sp::coordinates(plt)<- ~x+y
  # string WGS 84 / UTM zone 32N cartesian
  #epsg_in<-'+init=epsg:32632'
  # string ED50 / UTM zone 32N cartesian
  epsg_in<-'+init=epsg:23032'
  # set original coordinates system
  sp::proj4string(plt) <- sp::CRS(epsg_in)
  # set it as a grid
  sp::gridded(plt)<-TRUE
  # convert to a raster
  pltr <- raster::raster(plt)



  # define the binning
  #my_bins<-c(0, 1, 2, 3, 4, 5, 10, 100, round(max(raster::values(pltr)),0))
  #my_bins<-pretty(range(raster::values(pltr)))


  map_r<-mapview::mapview(pltr,
                          #here define the palette
                          col.regions = grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(8, 'RdYlBu'))),
                          #at = my_bins,
                          na.color ='transparent',
                          alpha.region= 0.5,
                          legend.opacity=0.5,
                          #query.digits=1,
                          query.position = 'bottomright',
                          #query.prefix = NULL,
                          layer.name = name_of_map_layer,
                          map.types = c('CartoDB.Positron','OpenStreetMap','Esri.WorldImagery')
  )

  save_mapview_html(map_r, string_filename_html)

}
