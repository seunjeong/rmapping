#' Get projection string
#'
#' @param code integer for EPSG code numbers
#'
#' @return
#' @export get_projection_string
#' @import rgdal
#' @import dplyr
#' @examples
get_projection_string <-function (code = 4326) {

  epsg <- make_EPSG() %>%
    dplyr::filter(code == 4326) %>% pull (prj4)

  return (epsg)

}
